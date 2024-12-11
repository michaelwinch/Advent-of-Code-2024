module Advent_of_Code.Program

type Run =
    | Actual of day: int
    | Example of day: int * example: int option

    static member toFilePath =
        function
        | Actual day -> $"Actual/Day{day}.txt"
        | Example (day, None) -> $"Examples/Day{day}.txt"
        | Example (day, Some example) -> $"Examples/Day{day}_{example}.txt"
        >> (+) "../../../../Inputs/"

    static member run name f numberOfRuns run =
        let filePath = Run.toFilePath run
        for _ in [1..numberOfRuns] do
            let sw = System.Diagnostics.Stopwatch.StartNew ()
            let result = f filePath
            let elapsedMs = sw.ElapsedMilliseconds
            printfn "%s completed in %dms with result: %A" name elapsedMs result
            
    static member example (f, day: int, part: int, ?example: int, ?runs: int) =
        Example (day, example)
        |> Run.run $"Part {part} example" f (defaultArg runs 1)

    static member actual (f, day: int, part: int, ?runs: int) =
        Actual day
        |> Run.run $"Part {part} actual" f (defaultArg runs 10)

open Day11

[<EntryPoint>]
let main _ =
    Run.actual (Part2.run 75, day = 11, part = 2, runs = 50)
    0
    