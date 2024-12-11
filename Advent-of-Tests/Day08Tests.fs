module Advent_of_Tests.Day08Tests

open Xunit
open FsUnit.Xunit
open Advent_of_Code.Day08

[<Fact>]
let ``Part 1 example`` () =
    let result = Run.example (Part1.run, day = 8, example = 1)
    result.Result |> should equal 14
    result.Elapsed.TotalMilliseconds |> should be (lessThan 5.)

[<Fact>]
let ``Part 1 actual`` () =
    let result = Run.actual (Part1.run, day = 8)
    result.Result |> should equal 273
    result.Elapsed.TotalMilliseconds |> should be (lessThan 25.)

[<Fact>]
let ``Part 2 example`` () =
    let result = Run.example (Part2.run, day = 8, example = 1)
    result.Result |> should equal 34
    result.Elapsed.TotalMilliseconds |> should be (lessThan 5.)

[<Fact>]
let ``Part 2 actual`` () =
    let result = Run.actual (Part2.run, day = 8)
    result.Result |> should equal 1017
    result.Elapsed.TotalMilliseconds |> should be (lessThan 20.)
