module Advent_of_Tests.Day07Tests

open Xunit
open FsUnit.Xunit
open Advent_of_Code.Day07

[<Fact>]
let ``Part 1 example`` () =
    let result = Run.example (Part1.run, day = 7)
    result.Result |> should equal 3749L
    result.Elapsed.TotalMilliseconds |> should be (lessThan 5.)

[<Fact>]
let ``Part 1 actual`` () =
    let result = Run.actual (Part1.run, day = 7)
    result.Result |> should equal 1153997401072L
    result.Elapsed.TotalMilliseconds |> should be (lessThan 10.)

[<Fact>]
let ``Part 2 example`` () =
    let result = Run.example (Part2.run, day = 7)
    result.Result |> should equal 11387L
    result.Elapsed.TotalMilliseconds |> should be (lessThan 5.)

[<Fact>]
let ``Part 2 actual`` () =
    let result = Run.actual (Part2.run, day = 7)
    result.Result |> should equal 97902809384118L
    result.Elapsed.TotalMilliseconds |> should be (lessThan 60.)
