module Advent_of_Code.Day04

type WordSearch = char Grid
type PotentialWord = char list

let getWordSearch inputFile =
    File.readStream inputFile
    |> Grid.ofStringSeq

module Part1 =
    let getPotentialWords (wordSearch: WordSearch) (index: GridIndex) currentChar : PotentialWord list =
        let vertical =
            [ yield Some currentChar
              for move in [1..3] do
                let idx = index |> GridIndex.moveDownN move
                yield Grid.tryItem idx wordSearch ]
            |> List.chooseAll id

        let horizontal =
            [ yield Some currentChar
              for move in [1..3] do
                let idx = index |> GridIndex.moveRightN move
                yield Grid.tryItem idx wordSearch ]
            |> List.chooseAll id

        let diagonalRight =
            [ yield Some currentChar
              for move in [1..3] do
                let idx =
                    index
                    |> GridIndex.moveDownN move
                    |> GridIndex.moveRightN move
                yield Grid.tryItem idx wordSearch ]
            |> List.chooseAll id

        let diagonalLeft =
            [ yield Some currentChar
              for move in [1..3] do
                let idx =
                    index
                    |> GridIndex.moveDownN move
                    |> GridIndex.moveLeftN move
                yield Grid.tryItem idx wordSearch ]
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
        let wordSearch = getWordSearch inputFile

        wordSearch
        |> Grid.mapi (getPotentialWords wordSearch)
        |> Grid.collect id
        |> List.countIf isXmas


module Part2 =
    [<Struct>]
    type PotentialX =
        { Line1: char list
          Line2: char list }
        
    let getX (wordSearch: WordSearch) (index: GridIndex) currentChar : PotentialX option =
        if not (currentChar = 'A') then None
        else
            { Line1 =
                [ Grid.tryItem (GridIndex.move { X = -1; Y = -1 } index) wordSearch
                  Grid.tryItem (GridIndex.move { X = 1; Y = 1 } index) wordSearch ]
                |> List.choose id
              Line2 =
                [ Grid.tryItem (GridIndex.move { X = 1; Y = -1 } index) wordSearch
                  Grid.tryItem (GridIndex.move { X = -1; Y = 1 } index) wordSearch ]
                |> List.choose id }
            |> Some

    let isXmas (x: PotentialX) =
        let containsMS l = List.contains 'M' l && List.contains 'S' l
        containsMS x.Line1 && containsMS x.Line2

    let run inputFile =
        let wordSearch = getWordSearch inputFile

        wordSearch
        |> Grid.mapi (getX wordSearch)
        |> Grid.choose id
        |> List.countIf isXmas


// Run.example (Part1.run, day = 4, part = 1) // Part 1 example completed in 4ms with result: 18
// Run.actual (Part1.run, day = 4, part = 1) // Part 1 actual completed in 129ms with result: 2500
//
// Run.example (Part2.run, day = 4, part = 2) // Part 2 example completed in 3ms with result: 9
// Run.actual (Part2.run, day = 4, part = 2) // Part 2 actual completed in 7ms with result: 1933
