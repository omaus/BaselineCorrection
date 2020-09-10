module ArguBC

open Argu

type Argument =
    | FolderPath of string
    | Overwrite of bool
    interface IArgParserTemplate with
        member s.Usage =
            match s with
            | FolderPath _  -> "Path to the folder where the tool shall search for suite2p traces."
            | Overwrite _   -> "Overwrite existing files."

let parser = ArgumentParser.Create<Argument>(programName = "BaselineCorrection.exe")

let usage = parser.PrintUsage ()