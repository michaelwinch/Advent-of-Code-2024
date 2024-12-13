module Advent_of_Code.Day06

open Grid

open System.Collections.Generic

type Map = char Grid
type Coordinates = GridIndex

[<Struct>]
type Direction =
    | Up
    | Right
    | Down
    | Left

module Direction =
    let charList =
        [ '^'; '>'; 'v'; 'V'; '<' ]

    let ofChar =
        function
        | '^' -> Up
        | '>' -> Right
        | 'v' | 'V' -> Down
        | '<' -> Left
        | x -> failwithf "unknown direction %c" x

    let advanceCoordinates (coords: Coordinates) direction : Coordinates =
        match direction with
        | Up -> GridIndex.moveUp
        | Right -> GridIndex.moveRight
        | Down -> GridIndex.moveDown
        | Left -> GridIndex.moveLeft
        |> fun moveF -> moveF coords

    let turnRight =
        function
        | Up -> Right
        | Right -> Down
        | Down -> Left
        | Left -> Up

[<Literal>]
let obstacle = '#'

let getStartingCoords (map: Map) : Coordinates =
    Grid.findIndex (fun x -> List.contains x ['^']) map

let getStartingDirection (coords: Coordinates) (map: Map) =
    Grid.item coords map
    |> Direction.ofChar

let getMap inputFile =
    File.readStream inputFile
    |> Grid.ofStringSeq

module Part1 =
    let calculatePath startingCoords startingDirection (map: Map) =
        List.unfold
            (fun (coords, direction) ->
                let newCoords = Direction.advanceCoordinates coords direction
                match Grid.tryItem newCoords map with
                | None -> None
                | Some v when not (v = obstacle) ->
                    Some (newCoords, (newCoords, direction))
                | Some _ ->
                    let direction = Direction.turnRight direction
                    let newCoords = Direction.advanceCoordinates coords direction
                    match Grid.tryItem newCoords map with
                    | None -> None
                    | Some v when not (v = obstacle) ->
                        Some (newCoords, (newCoords, direction))
                    | Some _ -> failwith "2nd obstacle not handled"
            )
            (startingCoords, startingDirection)
        |> fun visited -> startingCoords :: visited

    let run inputFile =
        let map = getMap inputFile
        let startingCoords = getStartingCoords map
        let startingDirection = getStartingDirection startingCoords map

        map
        |> calculatePath startingCoords startingDirection
        |> List.distinct
        |> List.length


module Part2 =
    let getObstacleCoordinatesToTest startingCoords startingDirection map : Coordinates list =
        Part1.calculatePath startingCoords startingDirection map
        |> List.distinct
        |> List.except [startingCoords]

    type VisitedObstacles = HashSet<struct (Coordinates * Direction)>

    let hasLoop startingCoords startingDirection (map: Map) (obstacleCoords: Coordinates) =
        let map = Grid.updateAt obstacleCoords obstacle map
        let visitedObstacles : VisitedObstacles = HashSet()

        let rec loop coords direction =
            let newCoords = Direction.advanceCoordinates coords direction
            match Grid.tryItem newCoords map with
            | None -> false
            | Some v when not (v = obstacle) ->
                loop newCoords direction
            | Some _ ->
                let newVisit = visitedObstacles.Add (struct (coords, direction))
                if not newVisit then true
                else
                    let direction = Direction.turnRight direction
                    let newCoords = Direction.advanceCoordinates coords direction
                    match Grid.tryItem newCoords map with
                    | None -> false
                    | Some v when not (v = obstacle) ->
                        loop newCoords direction
                    | Some _ ->
                        let direction = Direction.turnRight direction
                        let newCoords = Direction.advanceCoordinates coords direction
                        loop newCoords direction

        loop startingCoords startingDirection

    let run inputFile =
        let map = getMap inputFile
        let startingCoords = getStartingCoords map
        let startingDirection = getStartingDirection startingCoords map

        map
        |> getObstacleCoordinatesToTest startingCoords startingDirection
        |> List.countIf (hasLoop startingCoords startingDirection map)

        
// Run.example (Part1.run, day = 6, part = 1) // Part 1 example completed in 1ms with result: 41
// Run.actual (Part1.run, day = 6, part = 1) // Part 1 example completed in 5ms with result: 4647
//
// Run.example (Part2.run, day = 6, part = 2) // Part 2 example completed in 1ms with result: 6
// Run.actual (Part2.run, day = 6, part = 2) // Part 2 actual completed in 3126ms with result: 1723
