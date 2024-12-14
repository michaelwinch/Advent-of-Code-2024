module Advent_of_Tests.Day14Tests

open Xunit
open FsUnit.Xunit
open Advent_of_Code
open Advent_of_Code.Grid
open Advent_of_Code.Day14

[<Fact>]
let ``Parse robot without negatives`` () =
    let input = "p=0,4 v=3,5"

    let expected =
        { Position = { X = 0; Y = 4 }
          Velocity = { X = 3; Y = 5 } }
        
    getRobot input
    |> should equal expected

[<Fact>]
let ``Parse robot with negatives`` () =
    let input = "p=0,4 v=-3,-5"

    let expected =
        { Position = { X = 0; Y = 4 }
          Velocity = { X = -3; Y = -5 } }

    getRobot input
    |> should equal expected

[<Fact>]
let ``Part 1 example`` () =
    let result = Run.example (Part1.run (11, 7) 100, day = 14)
    result.Result |> should equal 12
    result.Elapsed.TotalMilliseconds |> should be (lessThan 5.)

[<Fact>]
let ``Part 1 actual`` () =
    let result = Run.actual (Part1.run (101, 103) 100, day = 14)
    result.Result |> should equal 217328832
    result.Elapsed.TotalMilliseconds |> should be (lessThan 10.)

[<Fact>]
let ``Part 2 actual`` () =
    let result = Run.actual (Part2.run false (101, 103), day = 14)
    result.Result |> should equal 7412
    result.Elapsed.TotalSeconds |> should be (lessThan 12.)