module Advent_of_Code.Day07

type PartialEquation =
    { Answer: int64
      Parts: int64 list }

module PartialEquation =
    let ofString (x: string) =
        let matches =
            Regex.matches "(\d+)" x
            |> Seq.collect _.Captures
            |> Seq.map (_.Value >> int64)
            |> List.ofSeq
            
        { Answer = List.head matches
          Parts = List.tail matches }


module Part1 =
    let isPartialEquationSolvable (eq: PartialEquation) : bool =
        let rec loop acc =
            function
            | [] -> acc = eq.Answer
            | curr :: rest ->
                loop (acc + curr) rest
                || loop (acc * curr) rest

        loop (List.head eq.Parts) (List.tail eq.Parts)

    let run inputFile =
        File.readStream inputFile
        |> List.ofSeq
        |> List.map PartialEquation.ofString
        |> List.filter isPartialEquationSolvable
        |> List.map _.Answer
        |> List.sum


module Part2 =
    let isPartialEquationSolvable (eq: PartialEquation) : bool =
        let rec loop acc =
            function
            | [] -> acc = eq.Answer
            | curr :: rest ->
                loop (acc + curr) rest
                || loop (string acc + string curr |> int64) rest
                || loop (acc * curr) rest

        loop (List.head eq.Parts) (List.tail eq.Parts)

    let run inputFile =
        File.readStream inputFile
        |> List.ofSeq
        |> List.map PartialEquation.ofString
        |> List.filter isPartialEquationSolvable
        |> List.map _.Answer
        |> List.sum

        
// Run.example (Part1.run, day = 7, part = 1) // Part 1 example completed in 1ms with result: 3749L
// Run.actual (Part1.run, day = 7, part = 1) // Part 1 example completed in 8ms with result: 1153997401072L
//
// Run.example (Part2.run, day = 7, part = 2) // Part 2 example completed in 2ms with result: 11387L
// Run.actual (Part2.run, day = 7, part = 2) // Part 2 actual completed in 254ms with result: 97902809384118L
