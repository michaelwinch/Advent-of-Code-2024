module Advent_of_Tests.Day13Tests

open Xunit
open FsUnit.Xunit
open Advent_of_Code
open Advent_of_Code.Day13

[<Fact>]
let ``Parse claw machine`` () =
    let input =
        [ "Button A: X+94, Y+34"
          "Button B: X+22, Y-67"
          "Prize: X=8400, Y=5400" ]

    let expected =
        { A = { X = 94; Y = 34 }
          B = { X = 22; Y = -67 }
          Prize = { X = 8400; Y = 5400 } }
        
    getClawMachine input
    |> should equal expected
    ()

[<Fact>]
let ``Part 1 example`` () =
    let result = Run.example (Part1.run, day = 13, example = 1)
    result.Result |> should equal 480L
    result.Elapsed.TotalMilliseconds |> should be (lessThan 5.)

[<Fact>]
let ``Part 1 actual`` () =
    let result = Run.actual (Part1.run, day = 13)
    result.Result |> should equal 33209L
    result.Elapsed.TotalMilliseconds |> should be (lessThan 10.)

[<Fact>]
let ``Part 2 example`` () =
    let result = Run.example (Part2.run, day = 13, example = 1)
    result.Result |> should equal 875318608908L
    result.Elapsed.TotalMilliseconds |> should be (lessThan 5.)

[<Fact>]
let ``Part 2 actual`` () =
    let result = Run.actual (Part2.run, day = 13)
    result.Result |> should equal 83102355665474L
    result.Elapsed.TotalMilliseconds |> should be (lessThan 5.)