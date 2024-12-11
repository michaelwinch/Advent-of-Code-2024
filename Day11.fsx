#load "./Utils.fsx"

open Utils

type Stone = int64

let getStones inputFile : Stone list =
    File.readStream inputFile
    |> List.ofSeq
    |> List.collect (fun str -> str.Split ' ' |> List.ofArray)
    |> List.map int64

let evolveStone (stone: Stone) : Stone list =
    match stone with
    | 0L -> [ 1L ]
    | x when Int64.digits x % 2L = 0 ->
        let str = string x
        [ int64 (str.Substring(0, str.Length / 2))
          int64 (str.Substring(str.Length / 2)) ]
    | x -> [ x * 2024L ]


module Part1 =
    let rec blink remainingBlinks stones =
        if remainingBlinks = 0 then stones
        else
            stones
            |> List.collect evolveStone
            |> blink (remainingBlinks - 1)

    let run numberOfBlinks inputFile =
        let stones = getStones inputFile

        blink numberOfBlinks stones
        |> List.length


module Part2 =
    open System.Collections.Generic

    type RemainingBlinks = int
    type NumberOfStones = int64
    type StonesProduced = Dictionary<Stone * RemainingBlinks, NumberOfStones>

    let rec loop remainingBlinks (stones: Stone list) =
        let stonesProduced = StonesProduced()

        let rec calculateStonesProducedWithCaching (remainingBlinks: RemainingBlinks) (stones: Stone list) =
            if remainingBlinks = 0 then
                stones |> List.length |> int64
            else
                stones
                |> List.sumBy (fun stone ->
                    match stonesProduced.TryGetValue ((stone, remainingBlinks)) with
                    | true, n -> n
                    | false, _ ->
                        let n =
                            evolveStone stone
                            |> calculateStonesProducedWithCaching (remainingBlinks - 1)
                        stonesProduced.Add((stone, remainingBlinks), n)
                        n)

        calculateStonesProducedWithCaching remainingBlinks stones

    let run numberOfBlinks inputFile =
        let stones = getStones inputFile
        loop numberOfBlinks stones

        
Run.example (Part1.run 1, day = 11, part = 1, example = 1) // Part 1 example completed in 2ms with result: 7
Run.example (Part1.run 25, day = 11, part = 1, example = 2) // Part 1 example completed in 10ms with result: 55312
Run.actual (Part1.run 25, day = 11, part = 1) // Part 1 actual completed in 28ms with result: 172484

Run.actual (Part2.run 25, day = 11, part = 2) // Part 2 example completed in 3ms with result: 172484L
Run.actual (Part2.run 75, day = 11, part = 2) // Part 2 actual completed in 43ms with result: 205913561055242L
