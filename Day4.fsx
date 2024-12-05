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

module Part1 =
    let getWindows (allChars: char list list) y x currentChar : char list list =
        let vertical =
            [ yield Some currentChar
              for move in [1..3] do
                yield List2D.tryGet (y + move) x allChars
            ]
            |> List.chooseAll id

        let horizontal =
            [ yield Some currentChar
              for move in [1..3] do
                yield List2D.tryGet y (x + move) allChars
            ]
            |> List.chooseAll id

        let diagonalRight =
            [ yield Some currentChar
              for move in [1..3] do
                yield List2D.tryGet (y + move) (x + move) allChars
            ]
            |> List.chooseAll id

        let diagonalLeft =
            [ yield Some currentChar
              for move in [1..3] do
                yield List2D.tryGet (y + move) (x - move) allChars
            ]
            |> List.chooseAll id

        [ vertical
          horizontal
          diagonalRight
          diagonalLeft ]

    let private xmas = "XMAS".ToCharArray() |> List.ofArray
    let private samx = xmas |> List.rev

    let isXmas (window: char list) =
        window = xmas || window = samx

    let run inputFile =
        let allChars =
            getInput inputFile
            |> List.ofSeq
            |> List.map (_.ToCharArray() >> List.ofArray)

        allChars
        |> List2D.mapi (getWindows allChars)
        |> List.collect id
        |> List.collect id
        |> List.countIf isXmas

module Part2 =
    let getX (allChars: char list list) y x currentChar : char list list =
        if not (currentChar = 'A') then []
        else
            [ [ yield List2D.tryGet (y - 1) (x - 1) allChars
                yield List2D.tryGet (y + 1) (x + 1) allChars ]
              |> List.choose id

              [ yield List2D.tryGet (y - 1) (x + 1) allChars
                yield List2D.tryGet (y + 1) (x - 1) allChars ]
              |> List.choose id ]

    let isXmas (x: char list list) =
        let containsMS l = List.contains 'M' l && List.contains 'S' l
        match x with
        | [ a; b ] -> containsMS a && containsMS b
        | _ -> failwith "invalid X"

    let run inputFile =
        let allChars =
            getInput inputFile
            |> List.ofSeq
            |> List.map (_.ToCharArray() >> List.ofArray)

        allChars
        |> List2D.mapi (getX allChars)
        |> List.collect id
        |> List.filter (List.isEmpty >> not)
        |> List.countIf isXmas

Run.example (Part1.run, day = 4, part = 1, debug = true) // Part 1 example completed in 4ms with result: 18
Run.actual (Part1.run, day = 4, part = 1) // Part 1 actual completed in 73ms with result: 2500

Run.example (Part2.run, day = 4, part = 2, debug = true) // Part 2 example completed in 3ms with result: 9
Run.actual (Part2.run, day = 4, part = 2) // Part 2 actual completed in 7ms with result: 1933
