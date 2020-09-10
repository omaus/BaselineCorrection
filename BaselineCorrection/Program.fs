// Learn more about F# at http://fsharp.org

open FSharpAux
open AuxFunctions

[<EntryPoint>]
let main argv =
    let rec loop () =
        printfn "Enter the path to the folder where the tool shall search for suite2p traces:"
        let folderPath = System.Console.ReadLine ()
        printfn "\nOverwrite? [true/false]"
        let overwrite = 
            match String.allLowerCase (System.Console.ReadLine ()) with 
            | "true" -> true
            | "false" -> false
            | _ -> failwith "ERROR: Wrong input given."
        printfn "\ninputs are:\n\nfolderPath: %s\noverwrite:%b\n\nIs this correct? [y/n]" folderPath overwrite
        match String.allLowerCase (System.Console.ReadLine ()) with 
        | "y" -> 
            BaselineCorrection.correctBaselinePipeline overwrite folderPath
            printfn "\nPress any key to exit."
            ignore (System.Console.Read ())
            0 // return an integer exit code
        | "n" -> printfn ""; loop ()
        | _ -> failwith "ERROR: Wrong input given."
    loop ()