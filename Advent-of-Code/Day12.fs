module Advent_of_Code.Day12

open Grid

open System.Collections.Generic

type PlantType = char
type GardenPlots = PlantType Grid


type PlotsInRegion = HashSet<GridIndex>

let getGardenPlots inputFile : GardenPlots =
    File.readStream inputFile
    |> Grid.ofStringSeq


module Part1 =
    [<Struct>]
    type Region =
        { Area: int
          Perimeter: int }

    module Region =
        let empty =
            { Area = 0
              Perimeter = 0 }

        let add (x: Region) (y: Region) : Region =
            { Area = x.Area + y.Area
              Perimeter = x.Perimeter + y.Perimeter }

    let fillRegion gardenPlots startingPoint : Region * PlotsInRegion =
        let plotsInThisRegion = HashSet()
        let plantType = Grid.item startingPoint gardenPlots

        let rec loop (region: Region) currentCoords =
            if plotsInThisRegion.Contains currentCoords then
                region
            else
                plotsInThisRegion.Add currentCoords |> ignore

                let adjacent = GridIndex.getAdjacentIndexes currentCoords
                let sameRegion, differentRegion =
                    adjacent
                    |> List.filter (plotsInThisRegion.Contains >> not)
                    |> List.partition (fun idx -> Grid.tryItem idx gardenPlots = Some plantType)

                let region' =
                    sameRegion
                    |> List.map (loop Region.empty)
                    |> List.reduceWithZero Region.empty Region.add

                region
                |> Region.add region'
                |> Region.add { Area = 1; Perimeter = List.length differentRegion }

        loop Region.empty startingPoint, plotsInThisRegion

    let calculateRegions (gardenPlots: GardenPlots) =
        let plotsInRegions : PlotsInRegion = HashSet()

        gardenPlots
        |> Grid.mapi
            (fun index _ ->
                if plotsInRegions.Contains index then
                    None
                else
                    let region, plots = fillRegion gardenPlots index
                    plotsInRegions.UnionWith plots
                    Some region)
        |> Grid.chooseList id

    let calculateCosts =
        List.sumBy (fun x -> x.Area * x.Perimeter)

    let run inputFile =
        getGardenPlots inputFile
        |> calculateRegions
        |> calculateCosts


module Part2 =
    [<Struct>]
    type Region =
        { Area: int
          Sides: int }

    module Region =
        let empty =
            { Area = 0
              Sides = 0 }

        let add (x: Region) (y: Region) : Region =
            { Area = x.Area + y.Area
              Sides = x.Sides + y.Sides }

    [<Struct>]
    type Direction =
        | Up
        | Right
        | Down
        | Left

    module Direction =
        let isHorizontal x =
            x = Up || x = Down

        let isVertical = isHorizontal >> not

    type PerimetersInRegion = HashSet<GridIndex * Direction>
    
    let getAdjacentIndexes (index: GridIndex) =
        [ GridIndex.moveUp index, Up
          GridIndex.moveRight index, Right
          GridIndex.moveDown index, Down
          GridIndex.moveLeft index, Left ]

    let isAdjacent a b = b = a + 1 || b = a - 1

    let countConsecutive isAdjacent (xs: 'idx list) =
        let rec loop acc currentIdx =
            function
            | [] -> acc
            | nextIdx :: rest ->
                let acc =
                    if isAdjacent nextIdx currentIdx then acc
                    else acc + 1
                loop acc nextIdx rest

        match xs with
        | [] -> 0
        | h :: t -> loop 1 h t

    let countConsecutiveHorizontally (xs: (GridIndex * Direction) list) =
        xs
        |> List.filter (snd >> Direction.isHorizontal)
        |> List.groupBy (fun (idx, dir) -> idx.Y, dir)
        |> List.map (fun (_, xs) -> xs |> List.map (fst >> _.X) |> List.sort)
        |> List.sumBy (countConsecutive isAdjacent)

    let countConsecutiveVertically (xs: (GridIndex * Direction) list) =
        xs
        |> List.filter (snd >> Direction.isVertical)
        |> List.groupBy (fun (idx, dir) -> idx.X, dir)
        |> List.map (fun (_, ys) -> ys |> List.map (fst >> _.Y) |> List.sort)
        |> List.sumBy (countConsecutive isAdjacent)

    let countSides (perimeters: PerimetersInRegion) : int =
        let perimeters = perimeters |> List.ofSeq
        countConsecutiveHorizontally perimeters
        + countConsecutiveVertically perimeters

    let fillRegion gardenPlots startingPoint : Region * PlotsInRegion =
        let plotsInThisRegion = HashSet()
        let perimetersInRegion : PerimetersInRegion = HashSet()
        let plantType = Grid.item startingPoint gardenPlots

        let rec loop (areaAcc: int) currentCoords =
            let isNewPlot = plotsInThisRegion.Add currentCoords
            if not isNewPlot then
                areaAcc
            else
                let adjacent = getAdjacentIndexes currentCoords

                let sameRegion, differentRegion =
                    adjacent
                    |> List.filter (fst >> plotsInThisRegion.Contains >> not)
                    |> List.partition (fun (idx, _) -> Grid.tryItem idx gardenPlots = Some plantType)

                let connectedArea =
                    sameRegion
                    |> List.sumBy (fst >> loop 0)

                for _, dir in differentRegion do
                    perimetersInRegion.Add (currentCoords, dir) |> ignore

                areaAcc + connectedArea + 1

        let area = loop 0 startingPoint
        { Area = area
          Sides = countSides perimetersInRegion }, plotsInThisRegion

    let calculateRegions (gardenPlots: GardenPlots) =
        let plotsInRegions : PlotsInRegion = HashSet()

        gardenPlots
        |> Grid.mapi
            (fun index _ ->
                if plotsInRegions.Contains index then
                    None
                else
                    let region, plots = fillRegion gardenPlots index
                    plotsInRegions.UnionWith plots
                    Some region)
        |> Grid.chooseList id

    let calculateCosts =
        List.sumBy (fun x -> x.Area * x.Sides)

    let run inputFile =
        getGardenPlots inputFile
        |> calculateRegions
        |> calculateCosts


// Part 1 actual completed in 23ms with result: 1431440
// Part 2 actual completed in 31ms with result: 869070

