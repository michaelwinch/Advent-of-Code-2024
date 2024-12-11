module Advent_of_Tests.Day01Tests

open Xunit
open FsUnit.Xunit
open Advent_of_Code.Day01

[<Fact>]
let ``Part 1 example`` () =
    let result = Run.example (Part1.run, day = 1)
    result.Result |> should equal 11
    result.Elapsed.TotalMilliseconds |> should be (lessThan 5.)

[<Fact>]
let ``Part 1 actual`` () =
    let result = Run.actual (Part1.run, day = 1)
    result.Result |> should equal 1646452
    result.Elapsed.TotalMilliseconds |> should be (lessThan 5.)

[<Fact>]
let ``Part 2 example`` () =
    let result = Run.example (Part2.run, day = 1)
    result.Result |> should equal 31
    result.Elapsed.TotalMilliseconds |> should be (lessThan 5.)

[<Fact>]
let ``Part 2 actual`` () =
    let result = Run.actual (Part2.run, day = 1)
    result.Result |> should equal 23609874
    result.Elapsed.TotalMilliseconds |> should be (lessThan 5.)
