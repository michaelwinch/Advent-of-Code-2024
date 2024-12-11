module Advent_of_Tests.Day03Tests

open Xunit
open FsUnit.Xunit
open Advent_of_Code.Day03

[<Fact>]
let ``Part 1 example`` () =
    let result = Run.example (Part1.run, day = 3, example = 1)
    result.Result |> should equal 161
    result.Elapsed.TotalMilliseconds |> should be (lessThan 5.)

[<Fact>]
let ``Part 1 actual`` () =
    let result = Run.actual (Part1.run, day = 3)
    result.Result |> should equal 164730528
    result.Elapsed.TotalMilliseconds |> should be (lessThan 5.)

[<Fact>]
let ``Part 2 example`` () =
    let result = Run.example (Part2.run, day = 3, example = 2)
    result.Result |> should equal 48
    result.Elapsed.TotalMilliseconds |> should be (lessThan 5.)

[<Fact>]
let ``Part 2 actual`` () =
    let result = Run.actual (Part2.run, day = 3)
    result.Result |> should equal 70478672
    result.Elapsed.TotalMilliseconds |> should be (lessThan 5.)
