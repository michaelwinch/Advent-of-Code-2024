module Advent_of_Tests.Day12Tests

open System.Collections.Generic
open Xunit
open FsUnit.Xunit
open Advent_of_Code
open Advent_of_Code.Day12
open Advent_of_Code.Day12.Part1
open Advent_of_Code.Day12.Part2

[<Fact>]
let ``Part 1 fill region simple 1`` () =
    let plots =
        [ [ 'A'; 'A' ]
          [ 'B'; 'B' ] ]

    let region, plotsInRegion = Part1.fillRegion plots GridIndex.zero
    plotsInRegion |> List.ofSeq |> should equal [ {X=0;Y=0}; {X=1;Y=0} ]
    region |> should equal { Area = 2; Perimeter = 6 }

[<Fact>]
let ``Part 1 fill region simple 2`` () =
    let plots =
        [ [ 'A'; 'A'; 'A' ]
          [ 'A'; 'B'; 'B' ] ]

    let region, plotsInRegion = Part1.fillRegion plots GridIndex.zero
    plotsInRegion |> List.ofSeq |> should equal [ {X=0;Y=0}; {X=1;Y=0}; {X=2;Y=0}; {X=0;Y=01} ]
    region |> should equal { Area = 4; Perimeter = 10 }

[<Fact>]
let ``Part 1 fill region plot containing plot`` () =
    let plots =
        [ [ 'A'; 'A'; 'A' ]
          [ 'A'; 'B'; 'A' ]
          [ 'A'; 'A'; 'A' ] ]

    let region, _ = Part1.fillRegion plots GridIndex.zero
    region |> should equal { Area = 8; Perimeter = 16 }

[<Fact>]
let ``Part 1 calculate regions`` () =
    let plots =
        [ [ 'A'; 'A'; 'A' ]
          [ 'A'; 'B'; 'A' ]
          [ 'A'; 'A'; 'A' ] ]

    Part1.calculateRegions plots
    |> should equal [ { Area = 8; Perimeter = 16 }; { Area = 1; Perimeter = 4 } ]

[<Fact>]
let ``Part 1 example 1`` () =
    let result = Run.example (Part1.run, day = 12, example = 1)
    result.Result |> should equal 140
    result.Elapsed.TotalMilliseconds |> should be (lessThan 5.)

[<Fact>]
let ``Part 1 example 2`` () =
    let result = Run.example (Part1.run, day = 12, example = 2)
    result.Result |> should equal 772
    result.Elapsed.TotalMilliseconds |> should be (lessThan 5.)

[<Fact>]
let ``Part 1 example 3`` () =
    let result = Run.example (Part1.run, day = 12, example = 3)
    result.Result |> should equal 1930
    result.Elapsed.TotalMilliseconds |> should be (lessThan 5.)

[<Fact>]
let ``Part 1 actual`` () =
    let result = Run.actual (Part1.run, day = 12)
    result.Result |> should equal 1431440
    result.Elapsed.TotalMilliseconds |> should be (lessThan 30.)

[<Fact>]
let ``Part 2 calculate regions o`` () =
    let plots =
        [ [ 'A'; 'A'; 'A' ]
          [ 'A'; 'B'; 'A' ]
          [ 'A'; 'A'; 'A' ] ]

    Part2.calculateRegions plots
    |> should equal [ { Area = 8; Sides = 8 }; { Area = 1; Sides = 4 } ]

[<Fact>]
let ``Part 2 calculate regions +`` () =
    let plots =
        [ [ 'A'; 'A'; 'A'; 'A'; 'A' ]
          [ 'A'; 'A'; 'B'; 'A'; 'A' ]
          [ 'A'; 'B'; 'B'; 'B'; 'A' ]
          [ 'A'; 'A'; 'B'; 'A'; 'A' ]
          [ 'A'; 'A'; 'A'; 'A'; 'A' ] ]

    Part2.calculateRegions plots
    |> should equal [ { Area = 20; Sides = 16 }; { Area = 5; Sides = 12 } ]

[<Fact>]
let ``Part 2 countConsecutive simple 1`` () =
    Part2.countConsecutive Part2.isAdjacent []
    |> should equal 0

[<Fact>]
let ``Part 2 countConsecutive simple 2`` () =
    Part2.countConsecutive Part2.isAdjacent [6]
    |> should equal 1

[<Fact>]
let ``Part 2 countConsecutive simple 3`` () =
    Part2.countConsecutive Part2.isAdjacent [6;7;8]
    |> should equal 1

[<Fact>]
let ``Part 2 countConsecutive simple 4`` () =
    Part2.countConsecutive Part2.isAdjacent [3;6;7;8]
    |> should equal 2

[<Fact>]
let ``Part 2 countConsecutiveHorizontally single item h`` () =
    let perimeters =
        [ Up; Down; Left; Right ]
        |> List.map (fun x -> GridIndex.zero, x)

    Part2.countConsecutiveHorizontally perimeters
    |> should equal 2

[<Fact>]
let ``Part 2 countConsecutiveHorizontally single item z`` () =
    let perimeters =
        [ Up; Down; Left; Right ]
        |> List.map (fun x -> GridIndex.zero, x)

    Part2.countConsecutiveVertically perimeters
    |> should equal 2

[<Fact>]
let ``Part 2 count sides single item`` () =
    let perimeters =
        [ Up; Down; Left; Right ]
        |> List.map (fun x -> GridIndex.zero, x)
        |> HashSet

    Part2.countSides perimeters
    |> should equal 4

[<Fact>]
let ``Part 2 count sides horizontal line`` () =
    let perimeters =
        [ GridIndex.zero, Up
          GridIndex.zero, Left
          GridIndex.zero, Down
          GridIndex.moveRight GridIndex.zero, Up
          GridIndex.moveRight GridIndex.zero, Down
          GridIndex.moveRight GridIndex.zero, Right ]
        |> HashSet

    Part2.countSides perimeters
    |> should equal 4

[<Fact>]
let ``Part 2 count sides vertical line`` () =
    let perimeters =
        [ GridIndex.zero, Up
          GridIndex.zero, Left
          GridIndex.zero, Right
          GridIndex.moveDown GridIndex.zero, Left
          GridIndex.moveDown GridIndex.zero, Down
          GridIndex.moveDown GridIndex.zero, Right ]
        |> HashSet

    Part2.countSides perimeters
    |> should equal 4

[<Fact>]
let ``Part 2 count sides rectangle`` () =
    let perimeters =
        [ GridIndex.zero, Up
          GridIndex.zero, Left
          GridIndex.moveDown GridIndex.zero, Left
          GridIndex.moveDownN 2 GridIndex.zero, Down
          GridIndex.moveDownN 2 GridIndex.zero, Left
          GridIndex.moveRight GridIndex.zero, Up
          GridIndex.moveRight GridIndex.zero, Right
          GridIndex.moveDown <| GridIndex.moveRight GridIndex.zero, Right
          GridIndex.moveDownN 2 <| GridIndex.moveRight GridIndex.zero, Right
          GridIndex.moveDownN 2 <| GridIndex.moveRight GridIndex.zero, Down ]
        |> HashSet

    Part2.countSides perimeters
    |> should equal 4

[<Fact>]
let ``Part 2 example 1`` () =
    let result = Run.example (Part2.run, day = 12, example = 1)
    result.Result |> should equal 80
    result.Elapsed.TotalMilliseconds |> should be (lessThan 5.)

[<Fact>]
let ``Part 2 example 3`` () =
    let result = Run.example (Part2.run, day = 12, example = 3)
    result.Result |> should equal 1206
    result.Elapsed.TotalMilliseconds |> should be (lessThan 5.)

[<Fact>]
let ``Part 2 example 4`` () =
    let result = Run.example (Part2.run, day = 12, example = 4)
    result.Result |> should equal 236
    result.Elapsed.TotalMilliseconds |> should be (lessThan 5.)

[<Fact>]
let ``Part 2 example 5`` () =
    let result = Run.example (Part2.run, day = 12, example = 5)
    result.Result |> should equal 368
    result.Elapsed.TotalMilliseconds |> should be (lessThan 5.)

[<Fact>]
let ``Part 2 actual`` () =
    let result = Run.actual (Part2.run, day = 12)
    result.Result |> should equal 869070
    result.Elapsed.TotalMilliseconds |> should be (lessThan 40.)
