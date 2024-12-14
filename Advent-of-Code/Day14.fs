module Advent_of_Code.Day14

open System
open Grid

type Robot =
    { Position: GridIndex
      Velocity: GridIndex }

[<Struct>]
type Quadrant =
    | TopLeft
    | TopRight
    | BottomLeft
    | BottomRight
    | NoQuadrant

let getRobot input : Robot =
    Regex.matches "(-?\d+),(-?\d+)" input
    |> Seq.map (_.Groups >> _.Values >> Seq.tail >> Seq.map _.Value >> List.ofSeq)
    |> Seq.map (
        function
        | [ x; y ] -> { X = int x; Y = int y }
        | _ -> failwith "wrong number of matches")
    |> List.ofSeq
    |> function
       | [ pos; vel ] -> { Position = pos; Velocity = vel }
       | _ -> failwith "wrong number of matches"

let getRobots inputFile =
    File.readStream inputFile
    |> List.ofSeq
    |> List.map getRobot

let moveRobot (roomSizeX, roomSizeY) (robot: Robot) : Robot =
    let moveInAxis maxLength velocity position =
        let newPosition = position + velocity
        if newPosition < 0 then maxLength + newPosition
        else if newPosition >= maxLength then newPosition - maxLength
        else newPosition

    { robot with
        Position =
            { X = moveInAxis roomSizeX robot.Velocity.X robot.Position.X
              Y = moveInAxis roomSizeY robot.Velocity.Y robot.Position.Y } }

let rec moveRobotsNTimes roomSize n robots : Robot list =
    if n = 0 then robots
    else moveRobotsNTimes roomSize (n - 1) (robots |> List.map (moveRobot roomSize))

let calculateSafetyFactor (roomSizeX, roomSizeY) robots =
    let boundaryX = (roomSizeX - 1) / 2
    let boundaryY = (roomSizeY - 1) / 2
    
    let getQuadrant robot =
        if robot.Position.X < boundaryX then
            if robot.Position.Y < boundaryY then TopLeft
            elif robot.Position.Y > boundaryY then BottomLeft
            else NoQuadrant
        elif robot.Position.X > boundaryX then
            if robot.Position.Y < boundaryY then TopRight
            elif robot.Position.Y > boundaryY then BottomRight
            else NoQuadrant
        else NoQuadrant

    robots
    |> List.groupBy getQuadrant
    |> List.filter (fst >> (=) NoQuadrant >> not)
    |> List.map (snd >> List.length)
    |> List.reduce (*)

module Part1 =
    let run roomSize n inputFile =
        getRobots inputFile
        |> moveRobotsNTimes roomSize n
        |> calculateSafetyFactor roomSize


module Part2 =
    let visualise roomSize n robots =
        robots
        |> List.map _.Position
        |> Grid.visualise roomSize
        printfn "> %A" n

    let [<Literal>] sequenceMinLength = 20

    let hasSequence robots =
        let positions = robots |> List.map _.Position |> List.distinct
        
        let rec loop sequenceLength positionsInSequence position =
            if sequenceLength >= sequenceMinLength then true
            else
                let adjacentIndexes =
                    GridIndex.getAdjacentAndDiagonalIndexes position
                    |> List.filter (fun x -> positionsInSequence |> List.contains x |> not)

                positions
                |> List.tryFind (fun x -> adjacentIndexes |> List.contains x)
                |> function
                    | Some adjacent -> loop (sequenceLength + 1) (position :: positionsInSequence) adjacent
                    | None -> false

        positions
        |> List.exists (loop 0 [])
        
    let moveRobotsUntilSequence outputVisualisation roomSize robots : int =
        let rec loop secondsPassed robots =
            if hasSequence robots then
                if outputVisualisation then visualise roomSize secondsPassed robots
                secondsPassed
            else
                robots
                |> List.map (moveRobot roomSize)
                |> loop (secondsPassed + 1)

        loop 0 robots

    let run outputVisualisation roomSize inputFile =
        getRobots inputFile
        |> moveRobotsUntilSequence outputVisualisation roomSize


// Part 1 actual completed in 5ms with result: 217328832
// Part 2 actual completed in 10363ms with result: 7412
