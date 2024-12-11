module Advent_of_Tests.Day06Tests

open Xunit
open FsUnit.Xunit
open Advent_of_Code.Day06

[<Fact>]
let ``Part 1 example`` () =
    let result = Run.example (Part1.run, day = 6)
    result.Result |> should equal 41
    result.Elapsed.TotalMilliseconds |> should be (lessThan 5.)

[<Fact>]
let ``Part 1 actual`` () =
    let result = Run.actual (Part1.run, day = 6)
    result.Result |> should equal 4647
    result.Elapsed.TotalMilliseconds |> should be (lessThan 10.)

[<Fact>]
let ``Part 2 example`` () =
    let result = Run.example (Part2.run, day = 6)
    result.Result |> should equal 6
    result.Elapsed.TotalMilliseconds |> should be (lessThan 5.)

[<Fact>]
let ``Part 2 actual`` () =
    let result = Run.actual (Part2.run, day = 6)
    result.Result |> should equal 1723
    result.Elapsed.TotalSeconds |> should be (lessThan 3.5)
