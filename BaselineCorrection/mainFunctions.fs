﻿module BaselineCorrection

open FSharpAux
open FSharp.Stats
open AuxFunctions
open AuxFunctions.BasicFunction
open AuxFunctions.IO

/// <summary>Takes a trace and determines the baseline based on a given windowsize.</summary>
/// <param name="windowSize">The size of the window which is used to determine the baseline. If 0, the algorithm iterates through all windowSizes between 5 and 500 and takes the window with the smallest SD over the windowSize with the smallest SDs. Otherwise, it searches for the window with the smallest SD.</param>
/// <param name="trace">The trace which baseline shall be determined.</param>
/// <returns>The mean of the baseline as the mean of the window with the smallest SD.</returns>
let determineBaseline (windowSizes: int []) (trace: float []) =
    let currentDic = System.IO.Directory.GetCurrentDirectory ()
    let determineBaselineFunction windowSizes trace = 
        let windowSizesCalc =
            windowSizes
            |> Array.map (
                fun winS ->
                    async {
                        // printfn "Calculating window size %i" winS
                        return (
                            Array.windowed winS trace
                            |> Array.mapi (fun i win -> i, Seq.mean win, Seq.stDev win)
                            |> Array.sortBy (fun (i,mean,sD) -> sD)
                            |> Array.take 10
                            |> Array.map (fun (i,mean,sD) -> mean)
                            |> Seq.stDev
                            )
                        }
                ) 
                |> Async.Parallel 
                |> Async.RunSynchronously
        windowSizesCalc 
        |> Array.mapi (fun i sD -> i + 5, sD)
        |> Array.minBy snd
        |> fun (winS,sD) ->
            let log1 = sprintf "Optimal window size is %i with an SD of %f" winS sD
            printfn "%s" log1
            logReport true log1 currentDic
            Array.windowed winS trace
            |> Array.mapi (fun i win -> i, Seq.mean win, Seq.stDev win)
            |> Array.sortBy (fun (i,mean,sD) -> sD)
            |> Array.item 0
            |> fun (fstFrame,baseline,sd) ->
                let now = System.DateTime.Now
                let log2 = sprintf "Optimal window ranges from frame #%i to #%i, has an SD of %f and a mean (baseline) of %f\nDate: %i.%i.%i, %i:%i:%i" fstFrame (fstFrame + winS - 1) sd baseline now.Day now.Month now.Year now.Hour now.Minute now.Second
                printfn "%s" log2
                logReport true log2 currentDic
                fstFrame, fstFrame + winS, baseline
    if windowSizes = [||] then determineBaselineFunction [|5 .. 500|] trace
    else determineBaselineFunction windowSizes trace

/// <summary>Takes an array of suite2p trace, determines for each trace a window which resembles the baseline of the fluorescence trace, and subtracts all events in the spike trace that are lower than the highest spike in the baseline window.</summary>
/// <param name="saveFolder">The folder where the traces shall be saved.</param>
/// <param name="s2pTrace">The suite2p input, consisting of the fluorescence, the neuropil, the spike trace, and the iscell-information.</param>
/// <returns>An array of spike traces consisting only of the spikes higher than the largest spike in their respective baseline window.</returns>
let subtractBaselineSpikes windowSizes neuCoEff saveFolder plane (s2pTraces: Suite2pTrace []) =
    let path = if String.last saveFolder <> '\\' then saveFolder + @"\" else saveFolder
    let currentDic = System.IO.Directory.GetCurrentDirectory ()
    s2pTraces
    |> Array.choosei (
        let mutable iN = -1
        fun iO s2pTrace ->
            if fst s2pTrace.IsCell then 
                iN <- iN + 1
                Some (
                    let log1 = sprintf "\nDetermining spike trace #%i" (iO + 1)
                    printfn "%s" log1
                    logReport true log1 currentDic
                    FSharpAux.IO.FileIO.writeStringToFile 
                        (not (iN = 0))
                        (path + "traceIDs_plane" + (string plane) + ".tsv") 
                        (
                            if iN = 0 then 
                                sprintf "Old index (zero-based)\tOld index\tNew index (zero-based)\tNew index\n%i\t%i\t%i\t%i" iO (iO + 1) iN (iN + 1) 
                            else sprintf "%i\t%i\t%i\t%i" iO (iO + 1) iN  (iN + 1)
                        )
                    let noNeuropilTrace = (s2pTrace.FluorescenceTrace,s2pTrace.NeuropilTrace) ||> Array.map2 (fun vFluo vNeuropil -> vFluo - vNeuropil * neuCoEff)
                    let baselineFrame = determineBaseline windowSizes noNeuropilTrace
                    let filterle = Array.max s2pTrace.SpikeTrace.[fstOf3 baselineFrame .. sndOf3 baselineFrame - 1]
                    let log2 = sprintf "Highest baseline peak is %f" filterle
                    printfn "%s" log2
                    logReport true log2 currentDic
                    Array.map (fun v -> if v > filterle then v else 0.) s2pTrace.SpikeTrace
                )
            else None
    )
    |> Array.map (Array.fold (fun acc v -> acc + string v + "\t") "")
    |> fun strArr ->
        let log3 = sprintf "Writing to .tsv file in folder %s." saveFolder
        printfn "%s" log3
        logReport true log3 currentDic
        System.IO.File.WriteAllLines (path + "baselineCorrectedSpikeTraces_plane" + (string plane) + ".tsv", strArr)

/// Browses through the given folder as well as through all subFolders, determines the baseline of the suite2pTraces and saves them. If overwrite is true, already processed folders get processed again. windowSizes determines the sizes of the windows to check for the most representative baseline. neuCoeff is the factor multiplied with the neuropil trace when subtracted from the raw fluorescence trace. If neuCoeff is 0, no neuropil trace subtraction will be performed.
let correctBaselinePipeline overwrite windowSizes neuCoeff pathToFolder =
    let currentDic = System.IO.Directory.GetCurrentDirectory ()
    let stoppy = System.Diagnostics.Stopwatch ()
    stoppy.Start ()
    let folders = array (List.rev (IO.findSuite2pFolders pathToFolder))
    stoppy.Stop ()
    let foldersL = folders.Length
    let log1 = sprintf "\nFound %i folders. Time consumed: %i h, %i m, %i s. Beginning baseline correction" foldersL stoppy.Elapsed.Hours stoppy.Elapsed.Minutes stoppy.Elapsed.Seconds
    printfn "%s" log1
    logReport true log1 currentDic
    folders
    |> Array.iteri (
        fun i path ->
            if overwrite then
                let log2 = sprintf "\nFolder #%i of %i:\nProcessing folder %s" (i + 1) foldersL path
                printfn "%s" log2
                logReport true log2 currentDic
                System.IO.Directory.GetDirectories (path, "plane*")
                |> Array.iteri (
                    fun i plane ->
                        let log3 = sprintf "Processing plane %i\n" i
                        printfn "%s" log3
                        logReport true log3 currentDic
                        stoppy.Restart ()
                        IO.constructSuite2pTraces plane
                        |> subtractBaselineSpikes windowSizes neuCoeff path i
                        stoppy.Stop ()
                        let daytime = System.DateTime.Now
                        let log4 = sprintf "Time needed: %i h, %i m, %i s. Date: %i.%i.%i, %i:%i:%i" stoppy.Elapsed.Hours stoppy.Elapsed.Minutes stoppy.Elapsed.Seconds daytime.Day daytime.Month daytime.Year daytime.Hour daytime.Minute daytime.Second
                        printfn "%s" log4
                        logReport true log4 currentDic
                )
            else
                let log5 = sprintf "\nFolder #%i of %i:\nChecking path %s for processed .tsv files" (i + 1) foldersL path
                printfn "%s" log5
                logReport true log5 currentDic
                if (System.IO.Directory.GetFiles (path, "baselineCorrectedSpikeTraces*")).Length = 0 then
                    let log6 = sprintf "No files found. Processing folder %s" path
                    printfn "%s" log6
                    logReport true log6 currentDic
                    System.IO.Directory.GetDirectories (path, "plane*")
                    |> Array.iteri (
                        fun i plane ->
                            let log7 = sprintf "Processing plane %i\n" i
                            printfn "%s" log7
                            logReport true log7 currentDic
                            stoppy.Restart ()
                            IO.constructSuite2pTraces plane
                            |> subtractBaselineSpikes windowSizes neuCoeff path i
                            stoppy.Stop ()
                            let log8 = sprintf "Time needed: %i h, %i m, %i s." stoppy.Elapsed.Hours stoppy.Elapsed.Minutes stoppy.Elapsed.Seconds
                            printfn "%s" log8
                            logReport true log8 currentDic
                    )
                else 
                    let log9 = sprintf ".tsv files found. Folder already processed. Continuing with next folder\n"
                    printfn "%s" log9
                    logReport true log9 currentDic
    )