let tee f x = f x; x

let log x = printfn "LOG: %A" x
let logm m x = printfn "%s: %A" m x

type Run =
    | Actual of day: int
    | Example of day: int * example: int option

    static member toFilePath =
        function
        | Actual day -> $"Inputs/Actual/Day{day}.txt"
        | Example (day, None) -> $"Inputs/Examples/Day{day}.txt"
        | Example (day, Some example) -> $"Inputs/Examples/Day{day}_{example}.txt"

    static member run name f debug run =
        let filePath = Run.toFilePath run
        let runs = if debug then [1] else [1..3]
        for _ in runs do
            let sw = System.Diagnostics.Stopwatch.StartNew ()
            let result = f filePath
            let elapsedMs = sw.ElapsedMilliseconds
            printfn "%s completed in %dms with result: %A" name elapsedMs result

    static member example (f, day: int, part: int, ?example: int, ?debug: bool) =
        Example (day, example)
        |> Run.run $"Part {part} example" f (defaultArg debug true)

    static member actual (f, day: int, part: int, ?debug: bool) =
        Actual day
        |> Run.run $"Part {part} actual" f (defaultArg debug false)


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
    let digits (x: int) =
        if x = 0 then 1
        else
            x
            |> abs
            |> float
            |> log10
            |> int
            |> (+) 1

module Int64 =
    let digits (x: int64) =
        if x = 0 then 1L
        else
            x
            |> abs
            |> float
            |> log10
            |> int64
            |> (+) 1L


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


type Array2D<'a> = private Array2D of 'a array array

module Array2D =
    let init yLength xLength initialiser =
        [ 0 .. yLength - 1 ]
        |> List.map (fun y -> Array.init xLength (initialiser y))
        |> List.toArray
        |> Array2D

    let create (x: _ array array) =
        if x.Length = 0 || x |> Array.forall (_.Length >> (=) x[0].Length) then
            Array2D x
        else
            failwith "input array is jagged"

    let get y x (Array2D array) =
        array[y][x]

    let set (Array2D array) y x value =
        Array.set array[y] x value

    let findIndex predicate (Array2D array) =
        let y = Array.findIndex (Array.exists predicate) array
        let x = Array.findIndex predicate array[y]
        y, x

    let lengthY (Array2D array) =
        array.Length

    let lengthX (Array2D array) =
        array[0].Length

    let log (formatter: _ -> string) (Array2D array) =
        Array.map (Array.map formatter >> String.concat "") array
        |> Array.iter (printfn "%s")


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


type List2D<'a> = 'a list list

module List2D =
    let create (x: _ list list) : _ List2D =
        if x.Length = 0 || x |> List.forall (_.Length >> (=) x[0].Length) then
            x
        else
            failwith "input list is jagged"

    let get y x (list: _ List2D) =
        list[y][x]

    let tryGet y x (list: _ List2D) =
        list
        |> List.tryItem y
        |> Option.bind (List.tryItem x)

    let set y x value (list: _ List2D) =
        list
        |> List.mapi (fun idxY row ->
            if idxY = y then
                row
                |> List.mapi (fun idxX valX -> if idxX = x then value else valX)
            else
                row)

    let findIndex predicate (list: _ List2D) =
        let y = List.findIndex (List.exists predicate) list
        let x = List.findIndex predicate list[y]
        y, x

    let lengthY (list: _ List2D) =
        List.length list

    let lengthX (list: _ List2D) =
        List.length list[0]

    let mapi f =
        List.mapi (fun y row -> List.mapi (fun x value -> f y x value) row)

    let choose f =
        List.collect (List.choose f)

    let log (formatter: _ -> string) (list: _ List2D) =
        List.map (List.map formatter >> String.concat "") list
        |> List.iter log

    let ofArray2D (Array2D array) : _ List2D =
        array
        |> List.ofArray
        |> List.map List.ofArray

    let pivot (list: _ List2D) =
        let yLength = lengthY list
        let xLength = lengthX list

        let idxMap =
            [
                for y in [ 0 .. yLength - 1 ] do
                    for x in  [ 0 .. xLength - 1 ] do
                        yield (x, y), get y x list
            ]
            |> Map

        Array2D.init xLength yLength (fun y x -> Map.find (y, x) idxMap)
        |> ofArray2D

    let ofStringSeq : string seq -> char List2D =
        List.ofSeq
        >> List.map (_.ToCharArray() >> List.ofArray)

module ListOption =
    let ofList xs =
        if List.isEmpty xs then None
        else Some xs


// module Test =
//     let connect (separator: string) (prefix: string) (suffix: string) = $"{prefix}{separator}{suffix}"
//     let connectKebab = connect "-"
//     let connectKebab2 prefix suffix = connect "-" prefix suffix
//
//     let run () = ()
//
//     type Amount = { Value: int }
//     module Amount =
//         let sum = List.sumBy _.Value
//
//     type Account = { Id: string }
//     type Transaction = { AccountId: string; Amount: Amount }
//
//     let getIncomeByAccount =
//         List.groupBy _.AccountId
//         >> Map
//         >> Map.map (fun _ -> List.map _.Amount >> Amount.sum)

    // let getIncomeByAccount transactions =
    //     transactions
    //     |> List.groupBy _.AccountId
    //     |> Map
    //     |> List.ofSeq
    //     |> List.map (fun _ transactions -> transactions |> List.map _.Amount |> Amount.sum)
    //     |> List.map (fun _ -> List.map _.Amount >> Amount.sum)


    // let getIncome =
    //     List.map _.Amount
    //     >> List.filter ((<) 0)
    //     >> List.reduce (+)