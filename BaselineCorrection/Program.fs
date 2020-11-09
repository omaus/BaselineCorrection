// Learn more about F# at http://fsharp.org

open FSharpAux
open AuxFunctions

[<EntryPoint>]
let main argv =
    let currentDic = System.IO.Directory.GetCurrentDirectory ()
    let log1 = "Starting BaselineCorrection\nCreating log file"
    printfn "%s" log1
    IO.logReport false log1 currentDic
    let rec loop () =
        let log2 = "Checking for settings file"
        printfn "%s" log2
        IO.logReport true log2 currentDic
        IO.checkForSettingsFile currentDic
        System.Threading.Thread.Sleep 500
        let log3 = "Reading settings.txt"
        printfn "%s" log3
        IO.logReport true log3 currentDic
        let overwrite, neuCoeff, windowSizes, windowSizes' =
            System.IO.File.ReadAllLines (currentDic + @"\settings.txt")
            |> fun sArr ->
                let ov =
                    sArr 
                    |> Array.choose (fun s -> if String.contains "overwrite:" s then Some (String.splitS "overwrite: " s |> Array.item 1) else None) 
                    |> Array.item 0
                    |> fun s -> (if s = "true" then true else false)
                let nC =
                    sArr 
                    |> Array.choose (fun s -> if String.contains "neuCoeff:" s then Some (String.splitS "neuCoeff: " s |> Array.item 1) else None)
                    |> Array.item 0
                    |> float
                let wS, wSraw = 
                    sArr 
                    |> Array.choose (fun s -> if String.contains "windowSizes:" s then Some (String.splitS "windowSizes: " s |> Array.item 1) else None)
                    |> Array.item 0
                    |> fun s -> 
                        (if s = "" then [||]
                        elif String.contains ".." s then 
                            String.splitS " .. " s |> Array.map int |> fun iArr -> Array.init (iArr.[1] - iArr.[0] + 1) (fun i -> i + iArr.[0])
                        else int s |> fun i -> [|i|]),
                        s
                ov,nC,wS,wSraw
        let log4 = "Enter the path to the folder where the tool shall search for suite2p traces:"
        printfn "%s" log4
        IO.logReport true log4 currentDic
        let folderPath = System.Console.ReadLine ()
        let log5 = sprintf "\nInputs are:\n\nfolderPath: %s\noverwrite: %b\nneuCoeff: %f\nwindowSize(s): %s\n\nIs this correct? [y/n]" folderPath overwrite neuCoeff windowSizes'
        printfn "%s" log5
        IO.logReport true log5 currentDic
        match String.allLowerCase (System.Console.ReadLine ()) with 
        | "y" -> 
            BaselineCorrection.correctBaselinePipeline overwrite windowSizes neuCoeff folderPath
            let log6 = "\nPress any key to exit"
            printfn "%s" log6
            IO.logReport true log6 currentDic
            ignore (System.Console.Read ())
            0 // return an integer exit code
        | "n" -> printfn ""; loop ()
        | _ -> failwith "ERROR: Wrong input given."
    loop ()