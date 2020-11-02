// Learn more about F# at http://fsharp.org

open FSharpAux
open AuxFunctions

[<EntryPoint>]
let main argv =
    let currentDic = System.IO.Directory.GetCurrentDirectory ()
    let rec loop () =
        printfn "Checking for settings file"
        IO.checkForSettingsFile currentDic
        System.Threading.Thread.Sleep 500
        printfn "Reading settings.txt"
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
        printfn "Enter the path to the folder where the tool shall search for suite2p traces:"
        let folderPath = System.Console.ReadLine ()
        printfn "\nInputs are:\n\nfolderPath: %s\noverwrite: %b\nneuCoeff: %f\nwindowSize(s): %s\n\nIs this correct? [y/n]" folderPath overwrite neuCoeff windowSizes'
        match String.allLowerCase (System.Console.ReadLine ()) with 
        | "y" -> 
            BaselineCorrection.correctBaselinePipeline overwrite windowSizes neuCoeff folderPath
            printfn "\nPress any key to exit"
            ignore (System.Console.Read ())
            0 // return an integer exit code
        | "n" -> printfn ""; loop ()
        | _ -> failwith "ERROR: Wrong input given."
    loop ()