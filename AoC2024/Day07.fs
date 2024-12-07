namespace AoC2024

open Utils

module Day07 =
    let parseInput (input: string) : (int64 * (int64 array)) array =
        input
        |> lines
        |> Array.map (fun line ->
            let (a, b) = line |> colons |> twoArrayToTuple
            (int64 a, b |> words |> Array.map int64))

    let rec makeOperatorLists (useHidden: bool) (acc: char list list) (length: int) : char list list =
        if length = 0 then
            acc
        else
            let lists =
                if useHidden then
                    if acc.IsEmpty then
                        [ [ '+' ]; [ '*' ]; [ '|' ] ]
                    else
                        acc |> List.collect (fun l -> [ '+' :: l; '*' :: l; '|' :: l ])
                else if acc.IsEmpty then
                    [ [ '+' ]; [ '*' ] ]
                else
                    acc |> List.collect (fun l -> [ '+' :: l; '*' :: l ])

            makeOperatorLists useHidden lists (length - 1)

    let rec applyOperators
        (target: int64)
        (useHidden: bool)
        (acc: int64)
        (operators: char list)
        (operands: int64 list)
        : int64 =
        if acc > target then
            -1
        else
            match operators, operands with
            | [], _ -> acc
            | (op :: ops, n :: ns) ->
                if op = '+' then
                    applyOperators target useHidden (acc + n) ops ns
                elif op = '*' then
                    applyOperators target useHidden (acc * n) ops ns
                else
                    applyOperators target useHidden (int64 $"{acc}{n}") ops ns

    let canMakeEquation (useHidden: bool) ((total, operands): (int64 * (int64 array))) : bool =
        let operatorsLists =
            makeOperatorLists useHidden [] (operands.Length - 1) |> Array.ofList

        let o :: os = operands |> Array.toList

        operatorsLists
        |> Array.map (fun operatorList -> applyOperators total useHidden o operatorList os)
        |> Array.exists ((=) total)

    let part1 (input: string) : string =
        input
        |> parseInput
        |> Array.filter (canMakeEquation false)
        |> Array.map fst
        |> Array.sum
        |> string

    let part2 (input: string) : string =
        input
        |> parseInput
        |> Array.filter (canMakeEquation true)
        |> Array.map fst
        |> Array.sum
        |> string
