module Advent_of_Code.Day15

open Grid

let [<Literal>] empty = '.'
let [<Literal>] wall = '#'
let [<Literal>] box = 'O'
let [<Literal>] robot = '@'

type Map = char Grid

[<Struct>]
type Instruction =
    | Up
    | Right
    | Down
    | Left

module Instruction =
    let allChars = [ '^'; '>'; 'v'; '<' ]

    let ofChar =
        function
        | '^' -> Up
        | '>' -> Right
        | 'v' -> Down
        | '<' -> Left
        | x -> failwithf "invalid instruction '%c'" x

let getMapAndInstructions inputFile : Map * Instruction list =
    let allLines = File.readStream inputFile

    let map =
        allLines
        |> Seq.filter _.StartsWith(wall)
        |> Grid.ofStringSeq

    let instructions =
        allLines
        |> Seq.filter (fun str -> Instruction.allChars |> List.exists str.StartsWith)
        |> Seq.collect (_.ToCharArray() >> Array.map Instruction.ofChar)
        |> List.ofSeq

    map, instructions

let move instruction index =
    match instruction with
    | Up -> GridIndex.moveUp index
    | Right -> GridIndex.moveRight index
    | Down -> GridIndex.moveDown index
    | Left -> GridIndex.moveLeft index

let findNextSpace map instruction startIdx =
    let rec loop currentIdx =
        match Grid.item currentIdx map with
        | x when x = empty -> Some currentIdx
        | x when x = wall -> None
        | _ -> loop (move instruction currentIdx)

    loop startIdx

module Part1 =
    let applyInstruction map instruction =
        let robotIdx = Grid.findIndex ((=) robot) map
        let nextPosition = move instruction robotIdx

        match findNextSpace map instruction nextPosition with
        | None -> ()
        | Some spaceIdx -> Grid.scoochInPlace spaceIdx robotIdx map

        map

    let applyInstructions map instructions =
        let rec loop map =
            function
            | [] -> map
            | instruction :: rest ->
                loop (applyInstruction map instruction) rest

        loop map instructions

    let calculateGPSCoordinates idx = idx.Y * 100 + idx.X
    
    let run inputFile =
        getMapAndInstructions inputFile
        ||> applyInstructions
        |> Grid.mapi (fun idx value -> if value = box then Some idx else None)
        |> Grid.chooseList id
        |> List.sumBy calculateGPSCoordinates

module Part2 =
    let [<Literal>] boxLeft = '['
    let [<Literal>] boxRight = ']'
    
    let doubleWidth (map: Map) : Map =
        let doubleChar =
            function
            | x when x = '#' -> [|'#';'#'|]
            | x when x = 'O' -> [|'[';']'|]
            | x when x = '.' -> [|'.';'.'|]
            | x when x = '@' -> [|'@';'.'|]
            | _ -> failwith "invalid char"
            
        map |> Array.map (Array.collect doubleChar)

    let moveBoxesVertically direction robotIdx map =
        let rec isSafeToMoveTo currentPosition =
            match Grid.item currentPosition map with
            | x when x = boxLeft ->
                let boxLeftIdx, boxRightIdx = currentPosition, GridIndex.moveRight currentPosition
                isSafeToMoveTo (move direction boxLeftIdx) && isSafeToMoveTo (move direction boxRightIdx)
            | x when x = boxRight ->
                let boxLeftIdx, boxRightIdx = GridIndex.moveLeft currentPosition, currentPosition
                isSafeToMoveTo (move direction boxLeftIdx) && isSafeToMoveTo (move direction boxRightIdx)
            | x when x = empty -> true
            | x when x = wall -> false
            | x -> failwithf "invalid char when checking %c" x

        let rec moveBoxesLoop currentPosition =
            match Grid.item currentPosition map with
            | x when x = robot ->
                let nextPosition = move direction currentPosition
                moveBoxesLoop nextPosition
                Grid.swapInPlace nextPosition currentPosition map
            | x when x = boxLeft || x = boxRight ->
                let boxLeftIdx, boxRightIdx =
                    if x = boxLeft then currentPosition, GridIndex.moveRight currentPosition
                    else GridIndex.moveLeft currentPosition, currentPosition

                let nextLeftIdx = move direction boxLeftIdx
                let nextRightIdx = move direction boxRightIdx
                moveBoxesLoop nextLeftIdx
                moveBoxesLoop nextRightIdx
                Grid.swapInPlace nextLeftIdx boxLeftIdx map
                Grid.swapInPlace nextRightIdx boxRightIdx map
            | x when x = empty -> ()
            | x when x = wall -> failwith "invalid wall when moving"
            | x -> failwithf "invalid char when moving %c" x

        let isSafeToMove = isSafeToMoveTo (move direction robotIdx)
        if isSafeToMove then moveBoxesLoop robotIdx

    let applyInstruction map instruction =
        let robotIdx = Grid.findIndex ((=) robot) map
        let nextPosition = move instruction robotIdx

        if instruction = Left || instruction = Right then
            match findNextSpace map instruction nextPosition with
            | None -> ()
            | Some spaceIdx -> Grid.scoochInPlace spaceIdx robotIdx map
        else
            moveBoxesVertically instruction robotIdx map

        map

    let applyInstructions map instructions =
        let rec loop map =
            function
            | [] -> map
            | instruction :: rest ->
                loop (applyInstruction map instruction) rest

        loop map instructions

    let calculateGPSCoordinates idx = idx.Y * 100 + idx.X

    let run inputFile =
        let map, instructions = getMapAndInstructions inputFile
        let map = doubleWidth map
        applyInstructions map instructions
        |> Grid.mapi (fun idx value -> if value = boxLeft then Some idx else None)
        |> Grid.chooseList id
        |> List.sumBy calculateGPSCoordinates


// Part 1 actual completed in 26ms with result: 1475249
// Part 1 actual completed in 60ms with result: 1509724

