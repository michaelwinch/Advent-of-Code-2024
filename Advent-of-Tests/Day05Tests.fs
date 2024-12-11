module Advent_of_Tests.Day05Tests

open Xunit
open FsUnit.Xunit
open Advent_of_Code.Day05

[<Fact>]
let ``Part 1 example`` () =
    let result = Run.example (Part1.run, day = 5)
    result.Result |> should equal 143
    result.Elapsed.TotalMilliseconds |> should be (lessThan 5.)

[<Fact>]
let ``Part 1 actual`` () =
    let result = Run.actual (Part1.run, day = 5)
    result.Result |> should equal 4766
    result.Elapsed.TotalMilliseconds |> should be (lessThan 20.)

[<Fact>]
let ``Part 2 example`` () =
    let result = Run.example (Part2.run, day = 5)
    result.Result |> should equal 123
    result.Elapsed.TotalMilliseconds |> should be (lessThan 5.)

[<Fact>]
let ``Part 2 actual`` () =
    let result = Run.actual (Part2.run, day = 5)
    result.Result |> should equal 6257
    result.Elapsed.TotalMilliseconds |> should be (lessThan 60.)
