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

    static member run name f debug run =
        let filePath = Run.toFilePath run
        let runs = if debug then 1 else 10
        for _ in [1..runs] do
            let sw = System.Diagnostics.Stopwatch.StartNew ()
            let result = f filePath
            let elapsedMs = sw.ElapsedMilliseconds
            printfn "%s completed in %dms with result: %A" name elapsedMs result
            
    static member example (f, day: int, part: int, ?example: int, ?debug: bool) =
        Example (day, example)
        |> Run.run $"Part {part} example" f (defaultArg debug true)

    static member actual (f, day: int, part: int, ?debug: bool) =
        Actual day
        |> Run.run $"Part {part} actual" f (defaultArg debug false)


[<EntryPoint>]
let main _ =
    Run.actual (Day11.Part2.run 75, day = 11, part = 2)
    0
    