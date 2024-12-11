#load "./Utils.fsx"
#load "./GridUtils.fsx"

open Utils
open GridUtils


type AntinodeLocations = Set<GridIndex>

let frequencies = ['a'..'z'] @ ['A'..'Z'] @ ['0'..'9'] |> Set
let (|IsAntenna|_|) value =
    if Set.contains value frequencies then Some value
    else None

module Part1 =
    let calculateAntinode (startCoords: GridIndex) (endCoords: GridIndex) =
        { X = (endCoords.X - startCoords.X) + endCoords.X
          Y = (endCoords.Y - startCoords.Y) + endCoords.Y }

    let findAntinodes (map: char Grid) (startCoords: GridIndex) : char -> AntinodeLocations =
        function
        | IsAntenna startFrequency ->
            map
            |> Grid.mapi (fun endCoords -> // Could optimise this by ignoring 3/4 of the map that would have an antinode outside the map
                function
                | _ when startCoords = endCoords -> None
                | IsAntenna endFrequency when endFrequency = startFrequency ->
                    calculateAntinode startCoords endCoords
                    |> Some
                    |> Option.filter (GridIndex.isWithinGrid map)
                | _ -> None)
            |> List.collect id
            |> List.choose id
            |> Set
        | _ -> Set.empty

    let run inputFile =
        let map =
            File.readStream inputFile
            |> Grid.ofStringSeq

        map
        |> Grid.mapi (findAntinodes map)
        |> List.collect id
        |> List.reduce (+)
        |> Set.count


module Part2 =
    let calculateAntinodes gridLengths (startCoords: GridIndex) (endCoords: GridIndex) : AntinodeLocations =
        let xDelta = endCoords.X - startCoords.X
        let yDelta = endCoords.Y - startCoords.Y

        List.unfold
            (fun currCoords ->
                let nextCoords =
                    { currCoords with
                        X = currCoords.X + xDelta
                        Y = currCoords.Y + yDelta }

                if GridIndex.isWithinGridLengths gridLengths nextCoords then
                    Some (nextCoords, nextCoords)
                else None)
            endCoords
        |> fun xs -> endCoords :: xs
        |> Set

    let findAntinodes (map: char Grid) (startCoords: GridIndex) : char -> AntinodeLocations =
        let gridLengths = Grid.getLengths map
        function
        | IsAntenna startFrequency ->
            map
            |> Grid.mapi (fun endCoords -> // Could optimise this by ignoring 3/4 of the map that would have an antinode outside the map
                function
                | _ when startCoords = endCoords -> None
                | IsAntenna endFrequency when endFrequency = startFrequency ->
                    calculateAntinodes gridLengths startCoords endCoords
                    |> Some
                | _ -> None)
            |> List.collect id
            |> List.choose id
            |> List.reduce (+)
        | _ -> Set.empty

    let run inputFile =
        let map =
            File.readStream inputFile
            |> Grid.ofStringSeq

        map
        |> Grid.mapi (findAntinodes map)
        |> List.collect id
        |> List.reduce (+)
        |> Set.count

        
Run.example (Part1.run, day = 8, part = 1, example = 1) // Part 1 actual completed in 4ms with result: 14
Run.actual (Part1.run, day = 8, part = 1) // Part 1 actual completed in 17ms with result: 273

Run.example (Part2.run, day = 8, part = 2, example = 1) // Part 2 example completed in 4ms with result: 34
Run.actual (Part2.run, day = 8, part = 2) // Part 2 actual completed in 18ms with result: 1017
