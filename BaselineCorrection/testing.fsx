#r @"C:\Repos\omaus\BaselineCorrection\BaselineCorrection\bin\Debug\netcoreapp3.1\BaselineCorrection.dll"
#r @"C:\Repos\omaus\BaselineCorrection\BaselineCorrection\bin\Debug\netcoreapp3.1\FSharpAux.dll"
#r @"C:\Repos\omaus\BaselineCorrection\BaselineCorrection\bin\Debug\netcoreapp3.1\FSharpAux.IO.dll"

open FSharpAux
open AuxFunctions

let logReport running (startDate: System.DateTime) text folder =
    let folder = if String.last folder = '\\' then folder else folder + "\\"
    let filename = 
        if running then 
            System.IO.Directory.GetFiles (folder, "log*")
            |> Array.last
            |> fun p -> p.[String.findIndexBack '\\' p + 1 ..]
        else 
            sprintf "log_%i_%i_%i_%i.%i.txt" startDate.Year startDate.Month startDate.Day startDate.Hour startDate.Minute
    FSharpAux.IO.FileIO.writeStringToFile true (folder + filename) text

logReport true System.DateTime.Now "gadinean" @"C:\Users\revil\Google Drive\(Zwischenablage)"