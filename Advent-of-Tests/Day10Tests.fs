module Advent_of_Tests.Day10Tests

open Xunit
open FsUnit.Xunit
open Advent_of_Code.Day10

[<Fact>]
let ``Part 1 example`` () =
    let result = Run.example (Part1.run, day = 10)
    result.Result |> should equal 1
    result.Elapsed.TotalMilliseconds |> should be (lessThan 5.)

[<Fact>]
let ``Part 1 actual`` () =
    let result = Run.actual (Part1.run, day = 10)
    result.Result |> should equal 746
    result.Elapsed.TotalMilliseconds |> should be (lessThan 15.)

[<Fact>]
let ``Part 2 example`` () =
    let result = Run.example (Part2.run, day = 10)
    result.Result |> should equal 16
    result.Elapsed.TotalMilliseconds |> should be (lessThan 5.)

[<Fact>]
let ``Part 2 actual`` () =
    let result = Run.actual (Part2.run, day = 10)
    result.Result |> should equal 1541
    result.Elapsed.TotalMilliseconds |> should be (lessThan 15.)
