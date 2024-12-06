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

        let advanceCoordinates (y, x) =
            function
            | Up -> y - 1, x
            | Right -> y, x + 1
            | Down -> y + 1, x
            | Left -> y, x - 1

        let turnRight =
            function
            | Up -> Right
            | Right -> Down
            | Down -> Left
            | Left -> Up

let obstacle = '#'


module Part1 =
    let calculatePath (map: char List2D) =
        let startingCoords = List2D.findIndex (fun x -> List.contains x ['^']) map
        let startingDirection =
            List2D.get (fst startingCoords) (snd startingCoords) map
            |> Direction.ofChar

        List.unfold
            (fun (coords, direction) ->
                let y, x as newCoords = Direction.advanceCoordinates coords direction
                match List2D.tryGet y x map with
                | None -> None
                | Some v ->
                    if not (v = obstacle) then
                        Some (newCoords, (newCoords, direction))
                    else
                        let direction = Direction.turnRight direction
                        let x, y as newCoords = Direction.advanceCoordinates coords direction
                        match List2D.tryGet y x map with
                        | None -> None
                        | Some _ -> // Doesn't handle a second obstacle
                            Some (newCoords, (newCoords, direction))
            )
            (startingCoords, startingDirection)
        |> fun xs -> startingCoords :: xs

    let run inputFile =
        getInput inputFile
        |> List2D.ofStringSeq
        |> calculatePath
        |> List.distinct
        |> List.length


module Part2 =
    let getStartingCoords map = List2D.findIndex (fun x -> List.contains x ['^']) map

    let getStartingDirection (y, x) map =
        List2D.get y x map
        |> Direction.ofChar

    let hasLoop (map: char List2D) (obstacley, obstaclex) =
        let map = List2D.set obstacley obstaclex obstacle map
        let startingCoords = getStartingCoords map
        let startingDirection = getStartingDirection startingCoords map

        let visitedInDirection coords direction visited =
            visited
            |> Map.tryFind coords
            |> Option.map (List.contains direction)
            |> Option.defaultValue false

        let addVisited coords direction visited =
            visited
            |> Map.tryFind coords
            |> function
                | Some dirs -> Map.add coords (direction :: dirs) visited
                | None -> Map.add coords [direction] visited

        let rec loop visited coords direction =
            let y, x as newCoords = Direction.advanceCoordinates coords direction
            match List2D.tryGet y x map with
            | None -> false
            | Some v when not (v = obstacle) ->
                if visitedInDirection newCoords direction visited then true
                else loop (addVisited newCoords direction visited) newCoords direction
            | Some _ ->
                let direction = Direction.turnRight direction
                let y, x as newCoords = Direction.advanceCoordinates coords direction
                match List2D.tryGet y x map with
                | None -> false
                | Some v when not (v = obstacle) ->
                    if visitedInDirection newCoords direction visited then true
                    else loop (addVisited newCoords direction visited) newCoords direction
                | Some _ ->
                    let direction = Direction.turnRight direction
                    let newCoords = Direction.advanceCoordinates coords direction
                    if visitedInDirection newCoords direction visited then true
                    else loop (addVisited newCoords direction visited) newCoords direction

        loop Map.empty startingCoords startingDirection

    let run inputFile =
        let map =
            getInput inputFile
            |> List2D.ofStringSeq

        map
        |> List2D.mapi (fun y x v ->
            if v = obstacle || List.contains v Direction.charList then None
            else Some (y, x))
        |> List.collect id
        |> List.choose id
        |> List.countIf (hasLoop map)
        
Run.example (Part1.run, day = 6, part = 1) // Part 1 example completed in 1ms with result: 41
Run.actual (Part1.run, day = 6, part = 1) // Part 1 example completed in 3ms with result: 4647

Run.example (Part2.run, day = 6, part = 2) // Part 2 example completed in 9ms with result: 6
Run.actual (Part2.run, day = 6, part = 2) // Part 2 actual completed in 138051ms with result: 1723
