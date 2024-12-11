#load "./Utils.fsx"

open Utils

module Part1 =
    type Instruction =
        | Multiply of x: int * y: int

        static member execute =
            function
            | Multiply (x, y) -> x * y

    let getInstructions : string -> Instruction list =
        Regex.matches "mul\((\d+),(\d+)\)"
        >> List.ofSeq
        >> List.map (fun x -> Multiply (int x.Groups[1].Value, int x.Groups[2].Value))
    
    let run inputFile =
        File.readStream inputFile
        |> List.ofSeq
        |> List.collect getInstructions
        |> List.map Instruction.execute
        |> List.sum


module Part2 =
    type Instruction =
        | Multiply of x: int * y: int
        | Enable
        | Disable

    let getInstructions (input: string) : Instruction list =
        let multiplyInstructions =
            input
            |> Regex.matches "mul\((\d+),(\d+)\)"
            |> List.ofSeq
            |> List.map (fun x -> x.Index, Multiply (int x.Groups[1].Value, int x.Groups[2].Value))

        let enableInstructions =
            input
            |> Regex.matches "do\(\)"
            |> List.ofSeq
            |> List.map (fun x -> x.Index, Enable)

        let disableInstructions =
            input
            |> Regex.matches "don't\(\)"
            |> List.ofSeq
            |> List.map (fun x -> x.Index, Disable)

        multiplyInstructions @ enableInstructions @ disableInstructions
        |> List.sortBy fst
        |> List.map snd

    let run inputFile =
        File.readStream inputFile
        |> List.ofSeq
        |> List.collect getInstructions
        |> List.fold (fun (enabled, acc: int) instruction ->
            match instruction with
            | Enable -> true, acc
            | Disable -> false, acc
            | Multiply (x, y) ->
                let acc =
                    if enabled then (x * y) + acc
                    else acc
                enabled, acc)
            (true, 0)
        |> snd


Run.example (Part1.run, day = 3, part = 1, example = 1) // Part 1 example completed in 1ms with result: 161
Run.actual (Part1.run, day = 3, part = 1) // Part 1 actual completed in 1ms with result: 164730528

Run.example (Part2.run, day = 3, part = 2, example = 2) // Part 2 example completed in 1ms with result: 48
Run.actual (Part2.run, day = 3, part = 2) // Part 2 actual completed in 1ms with result: 70478672
