#load "./Utils.fsx"

open Utils

[<Struct>]
type DenseFile =
    { Id: int
      Size: int }

[<Struct>]
type DenseItem =
    | File of DenseFile
    | Space of size: int

type DenseDiskMap = DenseItem list
type FullDiskMap = int option list

let getDenseDiskMap inputFile : DenseDiskMap =
    File.readStream inputFile
    |> List.ofSeq
    |> List.collect (_.ToCharArray() >> List.ofArray)
    |> List.map (string >> int)
    |> List.mapi (fun idx size ->
        if idx % 2 = 0 then
            let fileId = if idx = 0 then 0 else idx / 2
            File { Id = fileId; Size = size }
        else
            Space size)
    
let getFullDiskMap (denseDiskMap: DenseItem list) : FullDiskMap =
    denseDiskMap
    |> List.collect (function
        | File file -> List.replicate file.Size (Some file.Id)
        | Space size -> List.replicate size None)

module Part1 =
    type RearrangedDiskMap = int list

    let getLastFileId (diskMap: FullDiskMap) : FullDiskMap * int option =
        let rec loop =
            function
            | [] -> [], None
            | Some _ as value :: rest -> List.rev rest, value
            | None :: rest -> loop rest

        loop (diskMap |> List.rev)

    let rearrangeDiskMap (diskMap: FullDiskMap) : RearrangedDiskMap =
        let rec loop rearranged =
            function
            | [] -> rearranged |> List.rev
            | Some id :: rest -> loop (id :: rearranged) rest
            | None :: rest ->
                let rest, lastFileId = getLastFileId rest
                match lastFileId with
                | None -> loop rearranged []
                | Some id -> loop (id :: rearranged) rest

        loop [] diskMap

    let calculateChecksum (diskMap: RearrangedDiskMap) : int64 =
        diskMap
        |> List.mapi (*)
        |> List.map int64
        |> List.sum

    let run inputFile =
        inputFile
        |> getDenseDiskMap
        |> getFullDiskMap
        |> rearrangeDiskMap
        |> calculateChecksum


module Part2 =
    type RearrangedDiskMap = DenseItem list

    let replaceFileWithSpace (diskMap: DenseDiskMap) (file: DenseFile) : DenseDiskMap =
        let rec loop unchanged =
            function
            | [] -> unchanged |> List.rev
            | File f :: Space nextSpace :: rest when f = file ->
                Space (f.Size + nextSpace) :: rest
                |> (@) (unchanged |> List.rev)
            | File f :: rest when f = file ->
                Space f.Size :: rest
                |> (@) (unchanged |> List.rev)
            | File _ as item :: rest ->
                loop (item :: unchanged) rest
            | Space prevSpace :: File f :: Space nextSpace :: rest when f = file ->
                Space (prevSpace + f.Size + nextSpace) :: rest
                |> (@) (unchanged |> List.rev)
            | Space prevSpace :: File f :: rest when f = file ->
                Space (prevSpace + f.Size) :: rest
                |> (@) (unchanged |> List.rev)
            | Space _ as item :: rest  ->
                loop (item :: unchanged) rest

        loop [] diskMap

    let putFileInFirstSpace (diskMap: DenseDiskMap) (file: DenseFile) : DenseDiskMap =
        let rec loop rearranged =
            function
            | [] -> diskMap
            | File f :: _ when f = file -> diskMap
            | File _ as item :: rest -> loop (item :: rearranged) rest
            | Space size as item :: rest when size < file.Size -> loop (item :: rearranged) rest
            | Space size :: rest when size = file.Size ->
                let rearranged = (File file :: rearranged) |> List.rev
                let rest = replaceFileWithSpace rest file
                rearranged @ rest
            | Space size :: rest ->
                let spaceLeft = size - file.Size
                let rearranged = (Space spaceLeft :: File file :: rearranged) |> List.rev
                let rest = replaceFileWithSpace rest file
                rearranged @ rest

        loop [] diskMap

    let rearrangeDiskMap (diskMap: DenseDiskMap) : RearrangedDiskMap =
        let rec loop rearranged =
            function
            | [] -> rearranged
            | file :: rest -> loop (putFileInFirstSpace rearranged file) rest

        diskMap
        |> List.choose (function File f -> Some f | _ -> None)
        |> List.rev
        |> loop diskMap

    let calculateChecksum (diskMap: FullDiskMap) : int64 =
        diskMap
        |> List.mapi (fun idx value -> value |> Option.map ((*) idx))
        |> List.choose id
        |> List.map int64
        |> List.sum

    let run inputFile =
        inputFile
        |> getDenseDiskMap
        |> rearrangeDiskMap
        |> getFullDiskMap
        |> calculateChecksum

        
Run.example (Part1.run, day = 9, part = 1, example = 1) // Part 1 example completed in 3ms with result: 1928L
Run.actual (Part1.run, day = 9, part = 1) // Part 1 actual completed in 20907ms with result: 6353658451014L

Run.example (Part2.run, day = 9, part = 2, example = 1) // Part 2 example completed in 6ms with result: 2858L
Run.actual (Part2.run, day = 9, part = 2) // Part 2 actual completed in 2092ms with result: 6382582136592L
