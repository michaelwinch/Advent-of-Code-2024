module Advent_of_Tests.Day11Tests

open Xunit
open FsUnit.Xunit
open Advent_of_Code.Day11

[<Fact>]
let ``Part 1 example 1`` () =
    let result = Run.example (Part1.run 1, day = 11, example = 1)
    result.Result |> should equal 7
    result.Elapsed.TotalMilliseconds |> should be (lessThan 10.)

[<Fact>]
let ``Part 1 example 2`` () =
    let result = Run.example (Part1.run 25, day = 11, example = 2)
    result.Result |> should equal 55312
    result.Elapsed.TotalMilliseconds |> should be (lessThan 15.)

[<Fact>]
let ``Part 1 actual`` () =
    let result = Run.actual (Part1.run 25, day = 11)
    result.Result |> should equal 172484
    result.Elapsed.TotalMilliseconds |> should be (lessThan 60.)

[<Fact>]
let ``Part 2 gives the same answer as part 1`` () =
    let result = Run.actual (Part2.run 25, day = 11)
    result.Result |> should equal 172484L
    result.Elapsed.TotalMilliseconds |> should be (lessThan 5.)

[<Fact>]
let ``Part 2 actual`` () =
    let result = Run.actual (Part2.run 75, day = 11)
    result.Result |> should equal 205913561055242L
    result.Elapsed.TotalMilliseconds |> should be (lessThan 60.)
