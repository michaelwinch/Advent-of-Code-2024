module Advent_of_Code.Day09

let sw = System.Diagnostics.Stopwatch()

[<Struct>]
type DenseFile =
    { Id: int
      Size: int }

[<Struct>]
type DenseItem =
    | File of DenseFile
    | Space of size: int

type DenseDiskMap = DenseItem array
type FullDiskMap = int option array

let getDenseDiskMap inputFile : DenseDiskMap =
    File.readStream inputFile
    |> Array.ofSeq
    |> Array.collect _.ToCharArray()
    |> Array.map (string >> int)
    |> Array.mapi (fun idx size ->
        if idx % 2 = 0 then
            let fileId = if idx = 0 then 0 else idx / 2
            File { Id = fileId; Size = size }
        else
            Space size)
    
let getFullDiskMap (denseDiskMap: DenseDiskMap) : FullDiskMap =
    denseDiskMap
    |> Array.collect (function
        | File file -> Array.replicate file.Size (Some file.Id)
        | Space size -> Array.replicate size None)

module Part1 =
    type RearrangedDiskMap = int array

    let rearrangeDiskMap (diskMap: FullDiskMap) : RearrangedDiskMap =
        let rec loop () =
            let firstSpaceIdx = Array.tryFindIndex Option.isNone diskMap
            let lastFileIdx = Array.tryFindIndexBack Option.isSome diskMap
            match firstSpaceIdx, lastFileIdx with
            | Some spaceIdx, Some fileIdx when spaceIdx < fileIdx ->
                Array.swapInPlace spaceIdx fileIdx diskMap
                loop ()
            | _ -> ()

        loop ()
        diskMap
        |> Array.choose id

    let calculateChecksum (diskMap: RearrangedDiskMap) : int64 =
        diskMap
        |> Array.mapi (*)
        |> Array.map int64
        |> Array.sum

    let run inputFile =
        inputFile
        |> getDenseDiskMap
        |> getFullDiskMap
        |> rearrangeDiskMap
        |> calculateChecksum


module Part2 =
    type RearrangedDiskMap = DenseItem array

    let swapFileAndSpace spaceIdx fileIdx (diskMap: DenseDiskMap) : DenseDiskMap =
        let spaceSize = match diskMap[spaceIdx] with Space size -> size | _ -> failwith "expecting space"
        let file = match diskMap[fileIdx] with File file -> file | _ -> failwith "expecting file"

        let diskMap =
            match Array.tryItem (fileIdx - 1) diskMap, Array.tryItem (fileIdx + 1) diskMap with
            | Some (Space spaceBefore), Some (Space spaceAfter) when spaceIdx < fileIdx - 1 ->
                Array.set diskMap (fileIdx - 1) (Space (spaceBefore + file.Size + spaceAfter))
                Array.removeManyAt fileIdx 2 diskMap
            | Some (Space spaceBefore), _ when spaceIdx < fileIdx - 1 ->
                Array.set diskMap (fileIdx - 1) (Space (spaceBefore + file.Size))
                Array.removeAt fileIdx diskMap
            | _, Some (Space spaceAfter) ->
                Array.set diskMap fileIdx (Space (file.Size + spaceAfter))
                Array.removeAt (fileIdx + 1) diskMap
            | _, _ ->
                Array.set diskMap fileIdx (Space file.Size)
                diskMap

        let diskMap =
            match spaceSize - file.Size with
            | 0 ->
                Array.set diskMap spaceIdx (File file)
                diskMap
            | spaceLeft ->
                Array.set diskMap spaceIdx (File file)
                Array.insertAfter spaceIdx (Space spaceLeft) diskMap

        diskMap

    let rearrangeDiskMap (diskMap: DenseDiskMap) : RearrangedDiskMap =
        let files = diskMap |> Array.choose (function File f -> Some f | _ -> None)

        Array.foldBack
            (fun file diskMap ->
                let firstSpaceIdx = Array.tryFindIndex (function Space s -> s >= file.Size | _ -> false) diskMap
                let fileIdx = Array.findIndex (function File f -> f = file | _ -> false) diskMap
                match firstSpaceIdx with
                | Some spaceIdx when spaceIdx < fileIdx ->
                    swapFileAndSpace spaceIdx fileIdx diskMap
                | _ -> diskMap)
            files
            diskMap

    let calculateChecksum (diskMap: FullDiskMap) : int64 =
        diskMap
        |> Array.mapi (fun idx value -> value |> Option.map ((*) idx))
        |> Array.choose id
        |> Array.sumBy int64

    let run inputFile =
        inputFile
        |> getDenseDiskMap
        |> rearrangeDiskMap
        |> getFullDiskMap
        |> calculateChecksum

        
// Part 1 actual completed in 707ms with result: 6353658451014L
// Part 2 actual completed in 349ms with result: 6382582136592L
