#load "./Utils.fsx"

open Utils

type Grid<'a> = 'a list list

[<Struct>]
type GridIndex =
    { X: int
      Y: int }

[<RequireQualifiedAccess>]
module GridIndex =
    let moveUp (index: GridIndex) = { index with Y = index.Y - 1 }
    let moveRight (index: GridIndex) = { index with X = index.X + 1 }
    let moveDown (index: GridIndex) = { index with Y = index.Y + 1 }
    let moveLeft (index: GridIndex) = { index with X = index.X - 1 }

[<RequireQualifiedAccess>]
module Grid =
    let validate (grid: _ Grid) : bool =
        match grid with
        | [] -> true
        | rows -> rows |> List.map List.length |> List.countDistinct = 1

    let ofStringSeq (xs: string seq) : char Grid =
        xs
        |> Seq.map (_.ToCharArray() >> List.ofArray)
        |> List.ofSeq

    /// Finds the first index where the predicate is true, searching by row then column.
    /// I.e. { X = 8; Y = 1 } will be chosen before { X = 2; Y = 2 }
    let findIndex predicate (grid: _ Grid) : GridIndex =
        let y = List.findIndex (List.exists predicate) grid
        let x = List.findIndex predicate grid[y]
        { X = x; Y = y }

    let item (index: GridIndex) (grid: 'a Grid) : 'a =
        grid[index.Y][index.X]

    let tryItem (index: GridIndex) (grid: 'a Grid) : 'a option =
        grid
        |> List.tryItem index.Y
        |> Option.bind (List.tryItem index.X)

    let map (f: 'a -> 'b) (grid: 'a Grid) : 'b Grid =
        grid
        |> List.map (fun row ->
            row
            |> List.map f)

    let mapi (f: GridIndex -> 'a -> 'b) (grid: 'a Grid) : 'b Grid =
        grid
        |> List.mapi (fun y row ->
            row
            |> List.mapi (fun x value -> f { X = x; Y = y } value))

    let updateAt (index: GridIndex) (value: 'a) (grid: 'a Grid) : 'a Grid =
        let newRow = List.updateAt index.X value grid[index.Y]
        List.updateAt index.Y newRow grid