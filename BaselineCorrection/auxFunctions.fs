namespace AuxFunctions

open System
open FSharpAux

module Array = // in F#Aux neiballern!
    /// Returns a new array containing only the elements of the input array for which the given predicate returns true.
    let filteri (predicate: int -> 'T -> bool) (array: 'T []) =
        let mutable i = -1
        Array.filter (
            fun x ->
                i <- i + 1
                predicate i x
        ) array
    /// Returns the length of an array containing only the elements of the input array for which the given predicate returns true.
    let filterLength (predicate: 'T -> bool) (array: 'T []) =
        let mutable counter = 0
        for i = 0 to array.Length - 1 do 
            if predicate array.[i] then counter <- counter + 1
        counter
    /// Returns the length of an array containing only the elements of the input array for which the given predicate returns true.
    let filteriLength (predicate: int -> 'T -> bool) (array: 'T []) =
        let mutable counter = 0
        for i = 0 to array.Length - 1 do 
            if predicate i array.[i] then counter <- counter + 1
        counter
    /// Applies the given function to each element of the array. Returns the array comprised of the results x for each element where the function returns Some x.
    let choosei (chooser: int -> 'T -> 'U Option) (array: 'T []) =
        checkNonNull "array" array    
        let inline subUnchecked startIndex count (array : _ []) =
            let res = Array.zeroCreate count
            if count < 64 then 
                for i = 0 to res.Length - 1 do res.[i] <- array.[startIndex + i]
            else Array.Copy (array, startIndex, res, 0, count)
            res
        let mutable i = 0
        let mutable first = Unchecked.defaultof<'U>
        let mutable found = false
        while i < array.Length && not found do
            let element = array.[i]
            match chooser i element with 
            | None -> i <- i + 1
            | Some b -> 
                first <- b
                found <- true                            
        if i <> array.Length then
            let chunk1 : 'U [] = Array.zeroCreate ((array.Length >>> 2) + 1)
            chunk1.[0] <- first
            let mutable count = 1            
            i <- i + 1                                
            while count < chunk1.Length && i < array.Length do
                let element = array.[i]                                
                match chooser i element with
                | None -> ()
                | Some b -> 
                    chunk1.[count] <- b
                    count <- count + 1                            
                i <- i + 1
            if i < array.Length then                            
                let chunk2 : 'U [] = Array.zeroCreate (array.Length-i)                        
                count <- 0
                while i < array.Length do
                    let element = array.[i]                                
                    match chooser i element with
                    | None -> ()
                    | Some b -> 
                        chunk2.[count] <- b
                        count <- count + 1                            
                    i <- i + 1
                let res : 'U [] = Array.zeroCreate (chunk1.Length + count)
                Array.Copy (chunk1, res, chunk1.Length)
                Array.Copy (chunk2, 0, res, chunk1.Length, count)
                res
            else subUnchecked 0 count chunk1                
        else Array.empty
    /// Returns an array with the indeces of the elements in the input array that satisfy the given predicate.
    let findIndeces (predicate: 'T -> bool) (array: 'T []) =
        let mutable counter = 0
        for i = 0 to array.Length - 1 do if predicate array.[i] then counter <- counter + 1
        let mutable outputArr = Array.zeroCreate counter
        counter <- 0
        for i = 0 to array.Length - 1 do if predicate array.[i] then outputArr.[counter] <- i; counter <- counter + 1
        outputArr
    /// Returns a reversed array with the indeces of the elements in the input array that satisfy the given predicate.
    let findIndecesBack (predicate: 'T -> bool) (array: 'T []) =
        let mutable counter = 0
        for i = 0 to array.Length - 1 do if predicate array.[i] then counter <- counter + 1
        let mutable outputArr = Array.zeroCreate counter
        counter <- 0
        for i = array.Length - 1 downto 0 do
            if predicate array.[i] then 
                outputArr.[counter] <- i
                counter <- counter + 1
        outputArr
    /// Returns an array comprised of every nth element of the input array.
    let takeNth (n: int) (array: 'T []) = filteri (fun i _ -> (i + 1) % n = 0) array
    /// Returns an array without every nth element of the input array.
    let skipNth (n: int) (array: 'T []) = filteri (fun i _ -> (i + 1) % n <> 0) array


module String = // in F#Aux neiballern!
    /// Returns the first char of a string.
    let first (str:string) = str.Chars 0
    /// Returns the last char of a string.
    let last (str:string) = str.Chars (str.Length - 1)
    /// Splits an input string at a given delimiter (substring).
    let splitS (delimiter: string) (str: string) = str.Split ([|delimiter|],System.StringSplitOptions.None)
    /// Returns the last index of a char in a string.
    let findIndexBack (ch: char) (str: string) = str.ToCharArray () |> Array.findIndexBack (fun c -> c = ch)
    /// Returns the first index of a char in a string.
    let findIndex (ch: char) (str: string) = str.ToCharArray () |> Array.findIndex (fun c -> c = ch)
    /// Returns the indeces of a char in a string.
    let findIndeces (ch: char) (str: string) = str.ToCharArray () |> Array.findIndeces (fun c -> c = ch)
    /// Returns the indeces of a char in a string sorted backwards.
    let findIndecesBack (ch: char) (str: string) = str.ToCharArray () |> Array.findIndecesBack (fun c -> c = ch)
    /// Iterates through the string and returns a string with the chars of the input until the predicate returned false the first time.
    let takeWhile (predicate: char -> bool) (str: string) = 
        if String.IsNullOrEmpty str then str
        else
            let mutable i = 0
            while i < str.Length && predicate str.[i] do i <- i + 1
            String.take i str
    /// Iterates through the string and returns a string that starts at the char of the input where the predicate returned false the first time.
    let skipWhile (predicate: char -> bool) (str: string) =
        if String.IsNullOrEmpty str then str
        else
            let mutable i = 0
            while i < str.Length && predicate str.[i] do i <- i + 1
            String.skip i str


module BasicFunction =

    /// Creates an array of any given collection.
    let array collec = Array.ofSeq collec
    /// Creates a list of any given collection.
    let list collec = List.ofSeq collec


module IO =
    
    /// Browses the given folderPath and its subfolders and returns the paths to every suite2p-folder.
    let findSuite2pFolders folderPath =
        let mutable (listOfPaths: string list) = []
        let rec checkSub folder =
            printfn "\nAccessing folder %s" folder
            System.IO.Directory.GetDirectories folder
            |> fun subFolders ->
                if subFolders.Length = 0 then printfn "No subFolders in folder %s" folder
                else
                    printfn "Accessing subFolders"
                    Array.iter (
                        fun (subFolder:string) -> 
                            if subFolder.[subFolder.Length - 7 ..] = "suite2p" then printfn "\nsuite2p-Folder found. Adding to list"; listOfPaths <- subFolder::listOfPaths
                            checkSub subFolder
                    ) subFolders
        checkSub folderPath
        listOfPaths

    /// <summary>Reads a .npy file from the given file path and transforms it into an object.</summary>
    /// <param name="filePath">The full path to the .npy file.</param>
    /// <returns>An object. Since the nesting of the arrays of a .npy file can be different from file to file, the arrays are converted to object arrays which are upcasted to an object.</returns>
    /// <remarks>One has to downcast the object back to a nested object array and then convert the object type to string and after that to the desired datatype (e.g. float).</remarks>
    let inline readNpy filePath = 
        let byArr = System.IO.File.ReadAllBytes filePath
        let (infos, rawArr) = NPYReaderWriter.readNumpy byArr |> NPYReaderWriter.Result.requireOk
        let emptyArr: obj [] = Array.zeroCreate rawArr.Length
        rawArr.CopyTo (emptyArr, 0)
        let mutable obi : obj = null
        let rec destine depth array =
            if depth > 1 then
                printfn "%i" depth
                obi <- Array.chunkBySize infos.shape.[depth] array
                destine (depth - 1) (obi :?> obj [])
            else obi <- Array.chunkBySize infos.shape.[depth] array
        destine (infos.shape.Length - 1) emptyArr
        obi

    type Suite2pTrace = {
        IsCell              : bool * float
        FluorescenceTrace   : float []
        NeuropilTrace       : float []
        SpikeTrace          : float []
    }

    /// Loads the iscell-information, the flourescence, the neuropil, and the spike traces from a suite2p folder and returns an array of Suite2pTraces.
    let constructSuite2pTraces pathToFolder =
        let arr3d =
            [|"iscell.npy"; "F.npy"; "Fneu.npy"; "spks.npy"|]
            |> Array.map (
                fun fname -> 
                    ((System.IO.DirectoryInfo pathToFolder).GetFiles fname).[0].FullName
                    |> readNpy
                    :?> obj [] []
                    |> Array.map (Array.map (string >> float))
            )
        Array.init arr3d.[1].Length (
            fun celli -> 
                {
                    IsCell              = System.Convert.ToBoolean arr3d.[0].[celli].[0], arr3d.[0].[celli].[1]
                    FluorescenceTrace   = arr3d.[1].[celli]
                    NeuropilTrace       = arr3d.[2].[celli]
                    SpikeTrace          = arr3d.[3].[celli]
                }
        )