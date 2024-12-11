[<AutoOpen>]
module Advent_of_Tests.Utils

open System
open Advent_of_Code.Utils

type RunResult<'a> =
    { Result: 'a
      Elapsed: TimeSpan }

type Run =
    | Actual of day: int
    | Example of day: int * example: int option

    static member toFilePath =
        function
        | Actual day -> $"Actual/Day{day}.txt"
        | Example (day, None) -> $"Examples/Day{day}.txt"
        | Example (day, Some example) -> $"Examples/Day{day}_{example}.txt"
        >> (+) "../../../../Inputs/"

    static member run f numberOfRuns run =
        let filePath = Run.toFilePath run

        let results =
            [ for _ in [1..numberOfRuns] do
                let sw = System.Diagnostics.Stopwatch.StartNew ()
                { Result = f filePath
                  Elapsed = sw.Elapsed } ]

        if results |> List.map _.Result |> List.countDistinct |> (<) 1 then
            failwith "multiple different answers"
        else
            results |> List.minBy _.Elapsed

    static member example (f, day: int, ?example: int, ?runs: int) =
        Example (day, example)
        |> Run.run f (defaultArg runs 10)

    static member actual (f, day: int, ?runs: int) =
        Actual day
        |> Run.run f (defaultArg runs 10)
