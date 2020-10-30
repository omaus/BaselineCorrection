#r @"C:\Repos\omaus\BaselineCorrection\BaselineCorrection\bin\Debug\netcoreapp3.1\BaselineCorrection.dll"
#r @"C:\Repos\omaus\BaselineCorrection\BaselineCorrection\bin\Debug\netcoreapp3.1\FSharpAux.dll"

open FSharpAux
open AuxFunctions

let currentDic = @"C:\Repos\omaus\BaselineCorrection"

let overwrite, neuCoeff, windowSizes =
    System.IO.File.ReadAllLines (currentDic + @"\settings.txt")
    |> fun sArr ->
        sArr 
        |> Array.choose (fun s -> if String.contains "overwrite:" s then Some (String.splitS "overwrite: " s |> Array.item 1) else None) 
        |> Array.item 0
        |> fun s -> (if s = "true" then true else false),
        sArr 
        |> Array.choose (fun s -> if String.contains "neuCoeff:" s then Some (String.splitS "neuCoeff: " s |> Array.item 1) else None)
        |> Array.item 0
        |> float,
        sArr 
        |> Array.choose (fun s -> if String.contains "windowSizes:" s then Some (String.splitS "windowSizes: " s |> Array.item 1) else None)
        |> Array.item 0
        |> fun s -> 
            if s = "" then [||]
            elif String.contains ".." s then 
                String.splitS " .. " s |> Array.map int |> fun iArr -> Array.init (iArr.[1] - iArr.[0] + 1) (fun i -> i + iArr.[0])
            else int s |> fun i -> [|i|]