#time
#load "./Utils.fsx"

open System.IO
open Utils

let getInput (inputFile: string) =
    seq {
        use streamReader = new StreamReader(inputFile)
        while not streamReader.EndOfStream do
            yield streamReader.ReadLine()
    }

let getLocationIds : string seq -> int list * int list =
    Seq.toList
    >> List.map (fun x ->
        let xs = x.Split ' '
        int (Seq.head xs), int (Seq.last xs))
    >> List.unzip

module Part1 =
    let run inputFile =
        getInput inputFile
        |> getLocationIds
        |> fun (x, y) -> List.sort x, List.sort y
        ||> List.zip
        |> List.sumBy (fun (x, y) -> x - y |> abs)

module Part2 =
    let run inputFile =
        let xs, ys =
            getInput inputFile
            |> getLocationIds

        let yCountMap =
            ys
            |> List.groupBy id
            |> Map
            |> Map.mapValue List.length

        xs
        |> List.map (fun x ->
            Map.tryFind x yCountMap
            |> Option.defaultValue 0
            |> (*) x)
        |> List.sum


Run.example (Part1.run, day = 1, part = 1) // Part 1 example completed in 0ms with result: 11
Run.actual (Part1.run, day = 1, part = 1) // Part 1 actual completed in 0ms with result: 1646452

Run.example (Part2.run, day = 1, part = 2) // Part 2 example completed in 0ms with result: 31
Run.actual (Part2.run, day = 1, part = 2) // Part 2 actual completed in 1ms with result: 23609874
