//#r @"C:\Repos\omaus\BaselineCorrection\BaselineCorrection\bin\Debug\netcoreapp3.1\BaselineCorrection.dll"
//#r @"C:\Repos\omaus\BaselineCorrection\BaselineCorrection\bin\Debug\netcoreapp3.1\FSharpAux.dll"

//open FSharpAux
//open AuxFunctions

//let checkForSettingsFile =
//    //System.IO.Directory.GetCurrentDirectory ()
//    @"C:\Repos\omaus\BaselineCorrection\BaselineCorrection\bin\Release\netcoreapp3.1\publish"
//    |> fun s -> 
//        if System.IO.Directory.GetFiles (s, "settings.txt") = [||] then
//            let template = [|
//                "[Sets overwriting existing files to true or false (default)]\noverwrite: false\n"; 
//                "[Sets neuropil coefficient. Default is 0.7]\nneuCoeff: 0.7\n"; 
//                "[Sets window sizes to look for ideal baseline. Default is empty, meaning that all window sizes between 5 and 500 are checked. Either a single size (e.g.: 30) or a range of sizes (e.g.: 5 .. 100) can be set]\nwindowSizes: "
//            |]
//            System.IO.File.WriteAllLines (s + @"\settings.txt", template)

//checkForSettingsFile