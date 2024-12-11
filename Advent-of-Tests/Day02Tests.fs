module Advent_of_Tests.Day02Tests

open Xunit
open FsUnit.Xunit
open Advent_of_Code.Day02

[<Fact>]
let ``Part 1 example`` () =
    let result = Run.example (Part1.run, day = 2)
    result.Result |> should equal 2
    result.Elapsed.TotalMilliseconds |> should be (lessThan 5.)

[<Fact>]
let ``Part 1 actual`` () =
    let result = Run.actual (Part1.run, day = 2)
    result.Result |> should equal 236
    result.Elapsed.TotalMilliseconds |> should be (lessThan 5.)

[<Fact>]
let ``Part 2 example`` () =
    let result = Run.example (Part2.run, day = 2)
    result.Result |> should equal 4
    result.Elapsed.TotalMilliseconds |> should be (lessThan 5.)

[<Fact>]
let ``Part 2 actual`` () =
    let result = Run.actual (Part2.run, day = 2)
    result.Result |> should equal 308
    result.Elapsed.TotalMilliseconds |> should be (lessThan 10.)
