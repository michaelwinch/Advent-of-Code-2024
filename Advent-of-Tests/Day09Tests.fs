module Advent_of_Tests.Day09Tests

open Xunit
open FsUnit.Xunit
open Advent_of_Code.Day09

[<Fact>]
let ``Part 1 example`` () =
    let result = Run.example (Part1.run, day = 9, example = 1)
    result.Result |> should equal 1928L
    result.Elapsed.TotalMilliseconds |> should be (lessThan 5.)

[<Fact>]
let ``Part 1 actual`` () =
    let result = Run.actual (Part1.run, day = 9, runs = 1)
    result.Result |> should equal 6353658451014L
    result.Elapsed.TotalSeconds |> should be (lessThan 1.2)

[<Fact>]
let ``Part 2 example`` () =
    let result = Run.example (Part2.run, day = 9, example = 1)
    result.Result |> should equal 2858L
    result.Elapsed.TotalMilliseconds |> should be (lessThan 5.)

[<Fact>]
let ``Part 2 actual`` () =
    let result = Run.actual (Part2.run, day = 9, runs = 5)
    result.Result |> should equal 6382582136592L
    result.Elapsed.TotalSeconds |> should be (lessThan 1.)
