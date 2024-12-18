module Advent_of_Code.Grid

type Grid<'a> = 'a array array

[<Struct>]
type GridIndex =
    { X: int
      Y: int }

[<RequireQualifiedAccess>]
module GridIndex =
    let zero = { X = 0; Y = 0 }

    let move (moveBy: GridIndex) (index: GridIndex) =
        { index with
            X = index.X + moveBy.X
            Y = index.Y + moveBy.Y }

    let moveBack (moveBy: GridIndex) (index: GridIndex) =
        { index with
            X = index.X - moveBy.X
            Y = index.Y - moveBy.Y }

    let moveUp (index: GridIndex) = { index with Y = index.Y - 1 }
    let moveRight (index: GridIndex) = { index with X = index.X + 1 }
    let moveDown (index: GridIndex) = { index with Y = index.Y + 1 }
    let moveLeft (index: GridIndex) = { index with X = index.X - 1 }

    let moveUpN n (index: GridIndex) = { index with Y = index.Y - n }
    let moveRightN n (index: GridIndex) = { index with X = index.X + n }
    let moveDownN n (index: GridIndex) = { index with Y = index.Y + n }
    let moveLeftN n (index: GridIndex) = { index with X = index.X - n }

    let isWithinGrid (grid: _ Grid) (index: GridIndex) =
        index.Y >= 0
        && index.X >= 0
        && index.Y < Array.length grid
        && index.X < Array.length grid[0]

    let isWithinGridLengths (xLength: int, yLength: int) (index: GridIndex) =
        index.Y >= 0
        && index.X >= 0
        && index.Y < yLength
        && index.X < xLength

    let toString (index: GridIndex) = $"X{index.X}, Y{index.Y}"

    let getAdjacentIndexes (index: GridIndex) =
        [ moveUp index
          moveRight index
          moveDown index
          moveLeft index ]

    let getAdjacentAndDiagonalIndexes (index: GridIndex) =
        [ moveUp index
          moveRight index
          moveDown index
          moveLeft index
          move { X = -1; Y = -1 } index
          move { X = 1; Y = -1 } index
          move { X = -1; Y = 1 } index
          move { X = 1; Y = 1 } index ]


[<RequireQualifiedAccess>]
module Grid =
    let validate (grid: _ Grid) : bool =
        match grid with
        | [||] -> true
        | rows -> rows |> Array.map Array.length |> Array.countDistinct = 1

    let lengthX (grid: _ Grid) = Array.length grid[0]
    let lengthY (grid: _ Grid) = Array.length grid

    let getLengths (grid: _ Grid) = lengthX grid, lengthY grid

    let ofStringSeq (xs: string seq) : char Grid =
        xs
        |> Seq.map _.ToCharArray()
        |> Array.ofSeq

    /// Finds the first index where the predicate is true, searching by row then column.
    /// I.e. { X = 8; Y = 1 } will be chosen before { X = 2; Y = 2 }
    let findIndex predicate (grid: _ Grid) : GridIndex =
        let y = Array.findIndex (Array.exists predicate) grid
        let x = Array.findIndex predicate grid[y]
        { X = x; Y = y }

    let item (index: GridIndex) (grid: 'a Grid) : 'a =
        grid[index.Y][index.X]

    let tryItem (index: GridIndex) (grid: 'a Grid) : 'a option =
        grid
        |> Array.tryItem index.Y
        |> Option.bind (Array.tryItem index.X)

    let tryItemV (index: GridIndex) (grid: 'a Grid) : 'a ValueOption =
        tryItem index grid
        |> ValueOption.ofOption

    let map (f: 'a -> 'b) (grid: 'a Grid) : 'b Grid =
        grid
        |> Array.map (fun row ->
            row
            |> Array.map f)

    let mapi (f: GridIndex -> 'a -> 'b) (grid: 'a Grid) : 'b Grid =
        grid
        |> Array.mapi (fun y row ->
            row
            |> Array.mapi (fun x value -> f { X = x; Y = y } value))

    let set (grid: 'a Grid) index (value: 'a) : unit =
        Array.set grid[index.Y] index.X value

    let updateAt (index: GridIndex) (value: 'a) (grid: 'a Grid) : 'a Grid =
        let newRow = Array.updateAt index.X value grid[index.Y]
        Array.updateAt index.Y newRow grid

    let collectArray (f: 'a -> 'b array) (grid: 'a Grid) : 'b array =
        grid
        |> Array.collect id
        |> Array.collect f

    let collectList (f: 'a -> 'b list) (grid: 'a Grid) : 'b list =
        grid
        |> Array.collect id
        |> List.ofArray
        |> List.collect f

    let chooseArray (f: 'a -> 'b option) (grid: 'a Grid) : 'b array =
        grid
        |> Array.collect id
        |> Array.choose f

    let chooseList (f: 'a -> 'b option) (grid: 'a Grid) : 'b list =
        grid
        |> Array.collect id
        |> Array.choose f
        |> List.ofArray

    let swapInPlace indexA indexB (grid: _ Grid) =
        let a = item indexA grid
        set grid indexA (item indexB grid)
        set grid indexB a

    /// Swaps indexA to indexB and scooches all values in between down to the free space.
    /// Every value between indexA and indexB moves by 1.
    /// indexA and indexB must be on the same axis in one direction
    let scoochInPlace indexA indexB (grid: _ Grid) : unit =
        match indexA, indexB with
        | _, _ when indexA = indexB -> ()
        | { X = ax; Y = ay }, { X = bx; Y = by } when ax = bx ->
            let a = item indexA grid

            if ay < by then
                for i in [ ay + 1 .. by ] do
                    set grid { X = ax; Y = i - 1 } (item { X = ax; Y = i } grid)
            else
                for i in [ by .. ay - 1 ] |> List.rev do
                    set grid { X = ax; Y = i + 1 } (item { X = ax; Y = i } grid)

            set grid { X = ax; Y = by } a
        | { X = ax; Y = ay }, { X = bx; Y = by } when ay = by ->
            Array.scoochInPlace ax bx grid[ay]
        | _ -> failwith "cannot scooch as indexes are both on different axes"

    let test =
        [ "abcdef"
          "ghijkl"
          "mnopqr"
          "stuvwx" ]
        |> ofStringSeq

    scoochInPlace { X = 0; Y = 3 } { X = 0; Y = 1 } test

    let visualise (format: 'a -> char) (grid: 'a Grid) =
        grid
        |> Array.map (fun row -> row |> Array.map format |> System.String)
        |> Array.iter (printfn "%s")

    let visualiseIndexes (lengthX, lengthY) gridIndexes =
        let initPosition x y =
            gridIndexes
            |> List.countIf (fun idx -> idx.X = x && idx.Y = y)

        let format =
            function
            | 0 -> "."
            | x when x < 10 -> string x
            | _ -> "+"

        let initRow y = Array.init lengthX (fun x -> initPosition x y)
        Array.init lengthY initRow
        |> map format
        |> Array.map (Array.reduce (+))
        |> Array.iter (printfn "%s")

        gridIndexes
        |> List.filter (fun idx ->
            idx.X < 0 || idx.X >= lengthX
            || idx.Y < 0 || idx.Y >= lengthY)
        |> function
            | [] -> ()
            | xs -> printfn "Grid Indexes outside grid %A" xs
