module BaselineCorrection

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
    if windowSizes = [||] then
        let windowSizesCalc =
            [|5 .. 500|]
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
            printfn "Optimal window size is %i with an SD of %f" winS sD
            Array.windowed winS trace
            |> Array.mapi (fun i win -> i, Seq.mean win, Seq.stDev win)
            |> Array.sortBy (fun (i,mean,sD) -> sD)
            |> Array.item 0
            |> fun (fstFrame,baseline,sd) ->
                printfn "Optimal window ranges from frame #%i to #%i, has an SD of %f and a mean (baseline) of %f" fstFrame (fstFrame + winS - 1) sd baseline
                baseline
    else
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
            printfn "Optimal window size is %i with an SD of %f" winS sD
            Array.windowed winS trace
            |> Array.mapi (fun i win -> i, Seq.mean win, Seq.stDev win)
            |> Array.sortBy (fun (i,mean,sD) -> sD)
            |> Array.item 0
            |> fun (fstFrame,baseline,sd) ->
                printfn "Optimal window ranges from frame #%i to #%i, has an SD of %f and a mean (baseline) of %f" fstFrame (fstFrame + winS - 1) sd baseline
                baseline

/// <summary>Takes an array of suite2p trace, determines for each trace a window which resembles the baseline of the fluorescence trace, and subtracts all events in the spike trace that are 
/// lower than the highest spike in the baseline window.</summary>
/// <param name="saveFolder">The folder where the traces shall be saved.</param>
/// <param name="s2pTrace">The suite2p input, consisting of the fluorescence, the neuropil, the spike trace, and the iscell-information.</param>
/// <returns>An array of spike traces consisting only of the spikes higher than the largest spike in their respective baseline window.</returns>
let subtractBaselineSpikes windowSizes neuCoEff saveFolder plane (s2pTraces: Suite2pTrace []) =
    let path = if String.last saveFolder <> '\\' then saveFolder + @"\" else saveFolder
    s2pTraces
    |> Array.choosei (
        let mutable iN = -1
        fun iO s2pTrace ->
            if fst s2pTrace.IsCell then 
                iN <- iN + 1
                Some (
                    printfn "\nDetermining spike trace #%i" (iO + 1)
                    if saveFolder <> "" then 
                        FSharpAux.IO.FileIO.writeStringToFile 
                            true 
                            (path + "traceIDs_plane" + (string plane) + ".tsv") 
                            (
                                if iN = 0 then 
                                    sprintf "Old index (zero-based)\tOld index\tNew index (zero-based)\tNew index\n%i\t%i\t%i\t%i" iO (iO + 1) iN (iN + 1) 
                                else sprintf "%i\t%i\t%i\t%i" iO (iO + 1) iN  (iN + 1)
                            )
                    let baselineFrame =
                        let windowSizes =
                            [|5 .. 500|]
                            |> Array.map (
                                fun winS ->
                                    async {
                                        // printfn "Calculating window size %i" winS
                                        return (
                                            Array.windowed winS s2pTrace.FluorescenceTrace
                                            |> Array.mapi (fun i win -> i, Seq.mean win, Seq.stDev win)
                                            |> Array.sortBy (fun (i,mean,sD) -> sD)
                                            |> Array.truncate 10
                                            |> Array.map (fun (i,mean,sD) -> mean)
                                            |> Seq.stDev
                                        )
                                    }
                            ) 
                            |> Async.Parallel 
                            |> Async.RunSynchronously
                        windowSizes 
                        |> Array.mapi (fun i sD -> i + 5, sD) 
                        |> Array.minBy snd
                        |> fun (winS,sD) ->
                            printfn "Optimal window size is %i with an SD of %f" winS sD
                            Array.windowed winS s2pTrace.FluorescenceTrace
                            |> Array.mapi (fun i win -> i, Seq.mean win, Seq.stDev win)
                            |> Array.sortBy (fun (i,mean,sD) -> sD)
                            |> Array.item 0
                            |> fun (fstFrame,baseline,sd) ->
                                printfn "Optimal window ranges from frame #%i to #%i, has an SD of %f and a mean (baseline) of %f" fstFrame (fstFrame + winS - 1) sd baseline
                                fstFrame, (fstFrame + winS)
                    let filterle = Array.max s2pTrace.SpikeTrace.[fst baselineFrame .. snd baselineFrame - 1]
                    printfn "Highest baseline peak is %f" filterle
                    Array.map (fun v -> if v > filterle then v else 0.) s2pTrace.SpikeTrace
                )
            else None
    )
    |> Array.map (Array.fold (fun acc v -> acc + string v + "\t") "")
    |> fun strArr ->
        printfn "Writing to .tsv file in folder %s." saveFolder
        System.IO.File.WriteAllLines (path + "baselineCorrectedSpikeTraces_plane" + (string plane) + ".tsv", strArr)

/// Browses through the given folder as well as through all subFolders, determines the baseline of the suite2pTraces and saves them. If overwrite is true, already processed folders get processed again.
let correctBaselinePipeline overwrite pathToFolder =
    let stoppy = System.Diagnostics.Stopwatch ()
    let daytime = System.DateTime
    stoppy.Start ()
    let folders = array (List.rev (IO.findSuite2pFolders pathToFolder))
    stoppy.Stop ()
    let foldersL = folders.Length
    printfn "\nFound %i folders. Time consumed: %i h, %i m, %i s. Beginning baseline correction" foldersL stoppy.Elapsed.Hours stoppy.Elapsed.Minutes stoppy.Elapsed.Seconds
    folders
    |> Array.iteri (
        fun i path ->
            if overwrite then
                printfn "\nFolder #%i of %i:\nProcessing folder %s" (i + 1) foldersL path
                System.IO.Directory.GetDirectories (path, "plane*")
                |> Array.iteri (
                    fun i plane ->
                        printfn "Processing plane %i\n" i
                        stoppy.Restart ()
                        IO.constructSuite2pTraces plane
                        |> subtractBaselineSpikes path i
                        stoppy.Stop ()
                        let daytime = System.DateTime.Now
                        printfn "Time needed: %i h, %i m, %i s. Date: %i.%i.%i, %i:%i:%i" stoppy.Elapsed.Hours stoppy.Elapsed.Minutes stoppy.Elapsed.Seconds daytime.Day daytime.Month daytime.Year daytime.Hour daytime.Minute daytime.Second
                )
            else
                printfn "\nFolder #%i of %i:\nChecking path %s for processed .tsv files" (i + 1) foldersL path
                if (System.IO.Directory.GetFiles (path, "baselineCorrectedSpikeTraces*")).Length = 0 then
                    printfn "No files found. Processing folder %s" path
                    System.IO.Directory.GetDirectories (path, "plane*")
                    |> Array.iteri (
                        fun i plane ->
                            printfn "Processing plane %i\n" i
                            stoppy.Restart ()
                            IO.constructSuite2pTraces plane
                            |> subtractBaselineSpikes path i
                            stoppy.Stop ()
                            printfn "Time needed: %i h, %i m, %i s." stoppy.Elapsed.Hours stoppy.Elapsed.Minutes stoppy.Elapsed.Seconds
                    )
                else printfn ".tsv files found. Folder already processed. Continuing with next folder\n"
    )