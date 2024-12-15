module Advent_of_Tests.Day15Tests

open Xunit
open FsUnit.Xunit
open Advent_of_Code
open Advent_of_Code.Grid
open Advent_of_Code.Day15

[<Fact>]
let ``Part 1 example 1`` () =
    let result = Run.example (Part1.run, day = 15, example = 1)
    result.Result |> should equal 10092
    result.Elapsed.TotalMilliseconds |> should be (lessThan 5.)

[<Fact>]
let ``Part 1 example 2`` () =
    let result = Run.example (Part1.run, day = 15, example = 2)
    result.Result |> should equal 2028
    result.Elapsed.TotalMilliseconds |> should be (lessThan 5.)

[<Fact>]
let ``Part 1 actual`` () =
    let result = Run.actual (Part1.run, day = 15)
    result.Result |> should equal 1475249
    result.Elapsed.TotalMilliseconds |> should be (lessThan 40.)

[<Fact>]
let ``Part 2 example`` () =
    let result = Run.example (Part2.run, day = 15, example = 1)
    result.Result |> should equal 9021
    result.Elapsed.TotalMilliseconds |> should be (lessThan 5.)

[<Fact>]
let ``Part 2 actual`` () =
    let result = Run.actual (Part2.run, day = 15)
    result.Result |> should equal 1509724
    result.Elapsed.TotalMilliseconds |> should be (lessThan 140.)