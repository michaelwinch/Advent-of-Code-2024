[<AutoOpen>]
module Advent_of_Code.Utils

let tee f x = f x; x

let log x = printfn "LOG: %A" x
let logm m x = printfn "%s: %A" m x


module File =
    open System.IO

    let readStream (inputFile: string) =
        seq {
            use streamReader = new StreamReader(inputFile)
            while not streamReader.EndOfStream do
                yield streamReader.ReadLine()
        }


module String =
    open System

    let (|IsNullOrWhitespace|_|) (x: string) =
        if String.IsNullOrWhiteSpace x then Some () else None


module Int32 =
    open System

    let digits (x: int) =
        if x = 0 then 1
        else
            x
            |> abs
            |> float
            |> log10
            |> int
            |> (+) 1

    let ofInt64 (x: int64) =
        if x > (Int32.MaxValue |> int64) then
            failwithf "failed to convert int64 to int32, number too big"
        else int x


module Int64 =
    let digits (x: int64) =
        if x = 0 then 1
        else
            x
            |> abs
            |> float
            |> log10
            |> int
            |> (+) 1

    let (|NumberOfDigits|) = digits

    let concat (prefix: int64) (suffix: int64) =
        prefix * pown 10L (digits suffix) + suffix


module Maths =
    let highestCommonFactor x y =
        let lower, higher = if x < y then x, y else y, x
        let rec loop lower higher =
            if lower = 0L then higher
            else
                loop (higher % lower) lower
        loop lower higher

    let lowestCommonMultiple x y =
        x * y / highestCommonFactor x y


module Regex =
    open System.Text.RegularExpressions

    let ``match`` pattern input =
        Regex.Match(input, pattern).Groups

    let matches pattern input =
        Regex.Matches(input, pattern)

    let (|Match|_|) pattern input =
        let m = Regex.Match(input, pattern)
        if m.Success then Some m.Groups
        else None

    let (|Matches|_|) pattern input =
        let m = Regex.Matches(input, pattern)
        if m.Count > 0 then Some m else None

    let (|MatchesAlways|_|) pattern input =
        let m = Regex.Matches(input, pattern)
        Some m


module ValueOption =
    let ofOption =
        function
        | None -> ValueNone
        | Some x -> ValueSome x


module Array =
    let countIf (f: 'a -> bool) (xs: 'a array) =
        xs |> Array.filter f |> Array.length

    let countDistinct xs =
        xs |> Array.distinct |> Array.length

    let swapInPlace indexA indexB (xs: _ array) =
        let a = xs[indexA]
        Array.set xs indexA xs[indexB]
        Array.set xs indexB a

    /// Swaps indexA to indexB and scooches all values in between down to the free space.
    /// Every value between indexA and indexB moves by 1
    let scoochInPlace indexA indexB (xs: _ array) =
        if not (indexA = indexB) then
            let a = xs[indexA]

            if indexA < indexB then
                for i in [ indexA + 1 .. indexB ] do
                    Array.set xs (i - 1) xs[i]
            else
                for i in [ indexB .. indexA - 1 ] |> List.rev do
                    Array.set xs (i + 1) xs[i]

            Array.set xs indexB a

    let insertAfter index value xs =
        if index = Array.length xs - 1 then
            Array.append xs [| value |]
        else
            Array.insertAt (index + 1) value xs


module List =
    /// Except using the index
    let excepti exceptIdx xs =
        xs
        |> List.mapi (fun idx x -> if idx = exceptIdx then None else Some x)
        |> List.choose id

    let getUniquePairs (xs: _ list) =
        let rec loop acc =
            function
            | [] -> acc
            | h :: t ->
                let newPairs = t |> List.map (fun t -> h, t)
                loop (newPairs @ acc) t

        loop [] xs

    /// Returns a list of pairs of values, e.g. [ 1; 2; 3; 4 ] -> [ 1,2; 3,4 ].
    /// Will error if there is an odd number of values
    let pairwiseWithoutOverlaps (xs: _ list) =
        let rec loop acc =
            function
            | [] -> List.rev acc
            | [ _ ] -> failwith "must have an even number of elements"
            | a :: b :: rest ->
                loop ((a, b) :: acc) rest

        loop [] xs

    let chooseAll f (xs: _ option list) =
        let chosen =
            List.choose f xs

        if List.length chosen = List.length xs then chosen
        else []

    let countIf (f: 'a -> bool) (xs: 'a list) =
        xs |> List.filter f |> List.length

    let countDistinct xs =
        xs |> List.distinct |> List.length

    let insertAfter index value list =
        if index = List.length list - 1 then
            list @ [ value ]
        else
            List.insertAt (index + 1) value list

    let reduceWithZero zero reducer =
        function
        | [] -> zero
        | xs -> List.reduce reducer xs

    // From https://stackoverflow.com/questions/1526046/f-permutations
    let rec distribute e =
        function
        | [] -> [[e]]
        | x::xs' as xs -> (e::xs)::[for xs in distribute e xs' -> x::xs]

    // From https://stackoverflow.com/questions/1526046/f-permutations
    let rec permute =
        function
        | [] -> [[]]
        | e::xs -> List.collect (distribute e) (permute xs)


module Map =
    let mapValue f =
        Map.map (fun _ -> f)


module ListOption =
    let ofList xs =
        if List.isEmpty xs then None
        else Some xs
