module Advent_of_Code.Day13

open System
open Coordinates64

type ClawMachine =
    { A: Coordinates64
      B: Coordinates64
      Prize: Coordinates64 }

type Solution =
    { A: int64
      B: int64 }

module Solution =
    let cost solution =
        solution.A * 3L
        + solution.B


let (|Button|) input : Coordinates64 =
    let matches =
        Regex.matches "([+-]\d+)" input
        |> Seq.map (_.Groups >> _.Values >> Seq.item 1 >> _.Value)
        |> List.ofSeq

    match matches with
    | [x; y] ->
        { X = int x
          Y = int y }
    | _ -> failwithf "couldnt parse button %A" input
    
let (|Prize|) input : Coordinates64 =
    let matches =
        Regex.matches "(\d+)" input
        |> Seq.map (_.Groups >> _.Values >> Seq.item 1 >> _.Value)
        |> List.ofSeq

    match matches with
    | [x; y] ->
        { X = int64 x
          Y = int64 y }
    | _ -> failwithf "couldnt parse prize %A" input

let getClawMachine input =
    match input with
    | Button a :: Button b :: Prize p :: _ ->
        { A = a
          B = b
          Prize = p }
    | _ -> failwithf "could not parse claw machine %A" input

let getClawMachines inputFile =
    File.readStream inputFile
    |> List.ofSeq
    |> List.chunkBySize 4
    |> List.map getClawMachine

module Part1 =
    let solveSolution (clawMachine: ClawMachine) : int64 option =
        let startingBPresses =
            let maxX, _ = Math.DivRem(clawMachine.Prize.X, clawMachine.B.X)
            let maxY, _ = Math.DivRem(clawMachine.Prize.Y, clawMachine.B.Y)
            [ maxX; maxY ]
            |> List.min

        List.unfold
            (fun (bPresses, position) ->
                if bPresses < 0L then None
                else
                    let remainingDistance = Coordinates64.moveBack position clawMachine.Prize
                    let aPresses, remainderX = Math.DivRem(remainingDistance.X, clawMachine.A.X)
                    let solution =
                        if remainderX = 0 && aPresses * clawMachine.A.Y = remainingDistance.Y then
                            { A = aPresses; B = bPresses }
                            |> Some
                        else None
                    Some (Some solution, (bPresses - 1L, Coordinates64.moveBack clawMachine.B position))
            )
            (startingBPresses, { X = clawMachine.B.X * int64 startingBPresses; Y = clawMachine.B.Y * int64 startingBPresses })
        |> List.choose id
        |> List.choose id
        |> List.map Solution.cost
        |> ListOption.ofList
        |> Option.map List.min

    let run inputFile =
        getClawMachines inputFile
        |> List.choose solveSolution
        |> List.sum


module Part2 =
    let correctPrize (clawMachine: ClawMachine) : ClawMachine =
        { clawMachine with
            Prize.X = clawMachine.Prize.X + 10000000000000L
            Prize.Y = clawMachine.Prize.Y + 10000000000000L }

    let getSolution (clawMachine: ClawMachine) : Solution option =
        let aPresses, aRemainder =
            Math.DivRem (
                clawMachine.Prize.X * clawMachine.B.Y - clawMachine.Prize.Y * clawMachine.B.X,
                clawMachine.A.X * clawMachine.B.Y - clawMachine.A.Y * clawMachine.B.X
            )

        let bPresses, bRemainder =
            Math.DivRem (
                clawMachine.A.X * clawMachine.Prize.Y - clawMachine.A.Y * clawMachine.Prize.X,
                clawMachine.A.X * clawMachine.B.Y - clawMachine.A.Y * clawMachine.B.X
            )

        if aRemainder = 0 && bRemainder = 0 then
            { A = aPresses
              B = bPresses }
            |> Some
        else None

    let run inputFile =
        getClawMachines inputFile
        |> List.map correctPrize
        |> List.choose getSolution
        |> List.sumBy Solution.cost


// Part 1 actual completed in 8ms with result: 33209
// Part 2 actual completed in 2ms with result: 83102355665474L
