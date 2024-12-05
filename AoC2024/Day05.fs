namespace AoC2024

open Utils

module Day05 =

    let parseInput (input: string) : ((int * int) array) * int array array =
        let (rules, updates) = input |> splitAtDoubleLines |> twoArrayToTuple

        let processedRules =
            rules
            |> lines
            |> Array.map (split "|")
            |> Array.map (Array.map int)
            |> Array.map twoArrayToTuple

        let processedUpdates =
            updates
            |> lines
            |> Array.map (commas >> (Array.map int))

        (processedRules, processedUpdates)

    let pickRulesForUpdate (rules: (int * int) array) (update: int array) : (int * int) array =
        rules
        |> Array.filter (fun (a, b) ->
            (Array.contains a update)
            && (Array.contains b update))

    let isUpdateCorrect (rules: (int * int) array) (update: int array) : bool =
        rules
        |> Array.map (fun (a, b) -> Array.findIndex ((=) a) update < Array.findIndex ((=) b) update)
        |> Array.forall id

    let getMiddlePageNumber (update: int array) : int = update[(update.Length - 1) / 2]

    let part1 (input: string) : string =
        let (rules, updates) = input |> parseInput

        updates
        |> Array.filter (fun update ->
            let relevantRules = pickRulesForUpdate rules update
            isUpdateCorrect relevantRules update)
        |> Array.map getMiddlePageNumber
        |> Array.sum
        |> string

    let makePairs (update: int array) : ((int * int) * (int * int)) array =
        update
        |> Array.tail
        |> Array.mapi (fun i b -> (0, update[0]), (i + 1, b))

    let swapValues (array: 'a array) (indexA: int) (indexB: int) : 'a array =
        let newArray = Array.map id array
        let aVal = array[indexA]
        let bVal = array[indexB]
        Array.set newArray indexA bVal
        Array.set newArray indexB aVal

        newArray

    let swapIncorrect (rules: (int * int) array) (update: int array) : int array =
        let pairs = makePairs update

        let badPairOption =
            pairs
            |> Array.tryFind (fun ((_, a), (_, b)) -> rules |> Array.contains (b, a))

        match badPairOption with
        | None -> update
        | Some ((iA, _), (iB, _)) -> swapValues update iA iB

    let rec sortPass (rules: (int * int) array) (update: int array) : int array =
        match update with
        | [||] -> [||]
        | [| a |] -> [| a |]
        | arr ->
            let swapped = swapIncorrect rules arr
            Array.append [| swapped[0] |] (swapped |> Array.tail |> sortPass rules)

    let rec sortUntilCorrect (rules: (int * int) array) (update: int array) : int array =
        if isUpdateCorrect rules update then
            update
        else
            sortUntilCorrect rules (sortPass rules update)

    let part2 (input: string) : string =
        let (rules, updates) = input |> parseInput

        updates
        |> Array.filter (fun update ->
            let relevantRules = pickRulesForUpdate rules update
            not (isUpdateCorrect relevantRules update))
        |> Array.map (fun update ->
            let relevantRules = pickRulesForUpdate rules update
            sortUntilCorrect relevantRules update)
        |> Array.map getMiddlePageNumber
        |> Array.sum
        |> string
