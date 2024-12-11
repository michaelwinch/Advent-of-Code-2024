module Advent_of_Tests.Day04Tests

open Xunit
open FsUnit.Xunit
open Advent_of_Code.Day04

[<Fact>]
let ``Part 1 example`` () =
    let result = Run.example (Part1.run, day = 4)
    result.Result |> should equal 18
    result.Elapsed.TotalMilliseconds |> should be (lessThan 5.)

[<Fact>]
let ``Part 1 actual`` () =
    let result = Run.actual (Part1.run, day = 4)
    result.Result |> should equal 2500
    result.Elapsed.TotalMilliseconds |> should be (lessThan 150.)

[<Fact>]
let ``Part 2 example`` () =
    let result = Run.example (Part2.run, day = 4)
    result.Result |> should equal 9
    result.Elapsed.TotalMilliseconds |> should be (lessThan 5.)

[<Fact>]
let ``Part 2 actual`` () =
    let result = Run.actual (Part2.run, day = 4)
    result.Result |> should equal 1933
    result.Elapsed.TotalMilliseconds |> should be (lessThan 10.)
