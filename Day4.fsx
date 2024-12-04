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
    let getWindows (allChars: char list list) x y _ : char list list =
        let vertical =
            [ for move in [0..3] do
                yield List2D.tryGet (y + move) x allChars
            ]
            |> List.chooseAll id

        let horizontal =
            [ for move in [0..3] do
                yield List2D.tryGet y (x + move) allChars
            ]
            |> List.chooseAll id

        let diagonalRight =
            [ for move in [0..3] do
                yield List2D.tryGet (y + move) (x + move) allChars
            ]
            |> List.chooseAll id

        let diagonalLeft =
            [ for move in [0..3] do
                yield List2D.tryGet (y + move) (x - move) allChars
            ]
            |> List.chooseAll id

        [ vertical
          horizontal
          diagonalRight
          diagonalLeft ]

    let isXmas (window: char list) =
        window = ['X'; 'M'; 'A'; 'S']
        || window = ['S'; 'A'; 'M'; 'X']

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
                yield List2D.tryGet y x allChars
                yield List2D.tryGet (y + 1) (x + 1) allChars ]
              |> List.choose id

              [ yield List2D.tryGet (y - 1) (x + 1) allChars
                yield List2D.tryGet y x allChars
                yield List2D.tryGet (y + 1) (x - 1) allChars ]
              |> List.choose id ]

    let isXmas (x: char list list) =
        (x[0] = ['M'; 'A'; 'S'] || x[0] = ['S'; 'A'; 'M'])
        && (x[1] = ['M'; 'A'; 'S'] || x[1] = ['S'; 'A'; 'M'])

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

// Not the prettiest today, should refactor this when I'm not so tired