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

let getReports : string seq -> int list list =
    Seq.toList
    >> List.map (fun x ->
        x.Split ' '
        |> Array.filter (fun x -> x <> "")
        |> Array.map int
        |> List.ofArray)

type Direction =
    | Increasing
    | Decreasing

let getReportDirection (report: int list) =
    match report with
    | x :: y :: _ ->
        if x < y then Increasing else Decreasing
    | _ -> failwith "less than 2 levels in the report"

let areLevelsInTheCorrectDirection direction (x, y) =
    match direction with
    | Increasing -> x < y
    | Decreasing -> x > y

let areLevelsGraduallyChanging (x, y) =
    let delta = x - y |> abs
    1 <= delta && delta <= 3

let isReportSafe (report: int list) : bool =
    let direction = getReportDirection report
    report
    |> List.pairwise
    |> List.forall (fun pair ->
        areLevelsInTheCorrectDirection direction pair
        && areLevelsGraduallyChanging pair)

module Part1 =
    let run inputFile =
        getInput inputFile
        |> getReports
        |> List.countBy isReportSafe

module Part2 =
    let isReportSafeWithRemovals (report: int list) : bool =
        let rec loop levelIdxToRemove =
            match levelIdxToRemove with
            | [] -> false
            | removeIdx :: rest ->
                let isSafe =
                    report
                    |> List.excepti removeIdx
                    |> isReportSafe
                if isSafe then true
                else loop rest

        isReportSafe report
        || loop [ 0 .. report.Length - 1 ]

    let run inputFile =
        getInput inputFile
        |> getReports
        |> List.countBy isReportSafeWithRemovals

Run.example (Part1.run, day = 2, part = 1) // Part 1 example completed in 0ms with result: [(true, 2); (false, 4)]
Run.actual (Part1.run, day = 2, part = 1) // Part 1 actual completed in 1ms with result: [(false, 764); (true, 236)]

Run.example (Part2.run, day = 2, part = 2) // Part 2 example completed in 0ms with result: [(true, 4); (false, 2)]
Run.actual (Part2.run, day = 2, part = 2) // Part 2 actual completed in 2ms with result: [(true, 308); (false, 692)]
