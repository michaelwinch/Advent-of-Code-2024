#time
#load "./Utils.fsx"

open System
open System.IO
open Utils

let getInput (inputFile: string) =
    seq {
        use streamReader = new StreamReader(inputFile)
        while not streamReader.EndOfStream do
            yield streamReader.ReadLine()
    }

module Part1 =
    let run inputFile =
        getInput inputFile


Solution.run "Part 1" Part1.run "Inputs/Actual/Day1.txt"
