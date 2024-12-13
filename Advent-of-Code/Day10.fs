module Advent_of_Code.Day10

open Grid

type Map = Grid<int>

[<Struct>]
type CoordDetails =
    { Coords: GridIndex
      Height: int }

let getMap inputFile =
    File.readStream inputFile
    |> Grid.ofStringSeq
    |> Grid.map (string >> int)

let getTrailHeads : Map -> GridIndex list =
    Grid.mapi (fun coords height -> if height = 0 then Some coords else None)
    >> Grid.chooseList id
    
let findAdjacentGradualInclines (map: Map) height coords =
    [ GridIndex.moveUp coords
      GridIndex.moveRight coords
      GridIndex.moveDown coords
      GridIndex.moveLeft coords ]
    |> List.choose (fun coords ->
        Grid.tryItem coords map
        |> Option.map (fun height ->
            { Coords = coords
              Height = height }))
    |> List.filter (_.Height >> (=) (height + 1))

module Part1 =
    let getTrailHeadScore map trailHead =
        let rec loop height coords =
            let adjacentInclines = findAdjacentGradualInclines map height coords
            match adjacentInclines with
            | [] -> []
            | xs ->
                let maxHeight, notMax = xs |> List.partition (_.Height >> (=) 9)
                notMax
                |> List.collect (fun x -> loop x.Height x.Coords)
                |> (@) maxHeight

        loop 0 trailHead
        |> List.countDistinct

    let run inputFile =
        let map = getMap inputFile

        map
        |> getTrailHeads
        |> List.sumBy (getTrailHeadScore map)


module Part2 =
    let getTrailHeadScore map trailHead =
        let rec loop height coords =
            let adjacentInclines = findAdjacentGradualInclines map height coords
            match adjacentInclines with
            | [] -> []
            | xs ->
                let maxHeight, notMax = xs |> List.partition (_.Height >> (=) 9)
                notMax
                |> List.collect (fun x -> loop x.Height x.Coords)
                |> (@) maxHeight

        loop 0 trailHead
        |> List.length

    let run inputFile =
        let map = getMap inputFile

        map
        |> getTrailHeads
        |> List.sumBy (getTrailHeadScore map)

        
// Run.example (Part1.run, day = 10, part = 1) // Part 1 example completed in 1ms with result: 1
// Run.actual (Part1.run, day = 10, part = 1) // Part 1 actual completed in 8ms with result: 746
//
// Run.example (Part2.run, day = 10, part = 2) // Part 2 example completed in 1ms with result: 16
// Run.actual (Part2.run, day = 10, part = 2) // Part 2 actual completed in 8ms with result: 1541
