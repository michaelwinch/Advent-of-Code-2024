#load "./Utils.fsx"

open Utils

type OrderingRule =
    { X: int
      Y: int }

type UpdatePages = int list

module UpdatePages =
    let getMiddlePage (pages: UpdatePages) =
        let middleIdx = (pages.Length - 1) / 2
        pages[middleIdx]


let parseInput (input: string seq) : OrderingRule list * UpdatePages list =
    Seq.foldBack
        (fun line (rulesAcc, pagesAcc) ->
            match line with
            | Regex.Match "(\d+)\|(\d+)" groups ->
                let rule =
                    { X = int groups[1].Value
                      Y = int groups[2].Value }
                rule :: rulesAcc, pagesAcc
            | Regex.Matches "(?:(\d+),?)" matches ->
                let pages =
                    matches
                    |> List.ofSeq
                    |> List.map (_.Groups[1].Value >> int)

                rulesAcc, pages :: pagesAcc
            | _ -> rulesAcc, pagesAcc)
        input
        ([], [])

let getOrderingRulesAndUpdatePages inputFile =
    File.readStream inputFile
    |> parseInput

let isInCorrectOrder (rules: OrderingRule list) (pages: UpdatePages) =
    rules
    |> List.forall (fun rule ->
        let xIdx = pages |> List.tryFindIndex ((=) rule.X)
        let yIdx = pages |> List.tryFindIndex ((=) rule.Y)
        match xIdx, yIdx with
        | Some x, Some y -> x < y
        | _, _ -> true)

module Part1 =
    let run inputFile =
        let orderRules, updatePages = getOrderingRulesAndUpdatePages inputFile

        updatePages
        |> List.filter (isInCorrectOrder orderRules)
        |> List.sumBy UpdatePages.getMiddlePage


module Part2 =
    let reorderPass (rules: OrderingRule list) (pages: UpdatePages) : UpdatePages =
        rules
        |> List.fold (fun pages rule ->
            let yIdx = pages |> List.tryFindIndex ((=) rule.Y)
            let xIdx = pages |> List.tryFindIndex ((=) rule.X)
            match xIdx, yIdx with
            | Some xIdx, Some yIdx when yIdx < xIdx ->
                pages
                |> List.except [rule.Y]
                |> List.insertAfter (xIdx - 1) rule.Y
            | _, _ -> pages) // since y is removed, new xIdx will be 1 less
            pages

    let reorderUpdatePages (rules: OrderingRule list) (pages: UpdatePages) =
        let rec loop pages =
            let pages = reorderPass rules pages
            if isInCorrectOrder rules pages then pages
            else loop pages
        loop pages

    let run inputFile =
        let orderRules, updatePages = getOrderingRulesAndUpdatePages inputFile

        updatePages
        |> List.filter (isInCorrectOrder orderRules >> not)
        |> List.map (reorderUpdatePages orderRules)
        |> List.sumBy UpdatePages.getMiddlePage


Run.example (Part1.run, day = 5, part = 1) // Part 1 example completed in 1ms with result: 143
Run.actual (Part1.run, day = 5, part = 1) // Part 1 actual completed in 15ms with result: 4766

Run.example (Part2.run, day = 5, part = 2) // Part 2 example completed in 0ms with result: 123
Run.actual (Part2.run, day = 5, part = 2) // Part 2 actual completed in 49ms with result: 6257
