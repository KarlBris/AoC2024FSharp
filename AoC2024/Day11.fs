namespace AoC2024

open Utils

module Day11 =
    let memoLength = 5

    type MemoMap = Map<int64, (int64 * int64) array>

    let parseInput = words >> Array.map (fun n -> (1L, int64 n))

    let applyRules (stone: int64) : (int64 array) =
        if stone = 0 then
            [| 1L |]
        elif $"{stone}".Length % 2 = 0 then
            let ststr = $"{stone}"

            [| ststr.Substring(0, ststr.Length / 2) |> int64
               ststr.Substring(ststr.Length / 2) |> int64 |]
        else
            [| stone * 2024L |]

    let rec applyRulesManyTimes (times: int) (stones: int64 array) : (int64 array) =
        if times = 0 then
            stones
        else
            let result = Array.collect applyRules stones
            applyRulesManyTimes (times - 1) result

    let squish (stones: int64 array) : ((int64 * int64) array) =
        stones |> Array.groupBy id |> Array.map (fun (s, v) -> (v.Length, s))

    let applyRulesAndUpdateMemo (memoMap: MemoMap) ((times, stone): (int64 * int64)) : (MemoMap * (int64 * int64) array) =

        match memoMap |> Map.tryFind stone with
        | Some(v) -> (memoMap, v |> Array.map (fun (n, i) -> (n * times, i)))
        | None ->
            let memoResult = [| stone |] |> applyRulesManyTimes memoLength |> squish
            let mappedResult = memoResult |> Array.map (fun (n, s) -> (n * times, s))
            (memoMap |> Map.add stone memoResult, mappedResult)

    let squishMore (stones: (int64 * int64) array) : ((int64 * int64) array) =
        stones |> Array.groupBy snd |> Array.map (fun (s, v) -> (v |> Array.map fst |> Array.sum, s))

    let rec applyRulesToStonesAndUpdateMap
        (memoMap: MemoMap)
        (stoneAcc: (int64 * int64) list)
        (stones: (int64 * int64) list)
        : MemoMap * (int64 * int64) array =
        match stones with
        | [] ->
            let squishedResult = stoneAcc |> Array.ofList |> squishMore
            memoMap, squishedResult
        | s :: ss ->
            let (mm', ss') = applyRulesAndUpdateMemo memoMap s

            applyRulesToStonesAndUpdateMap mm' (List.append (List.ofArray ss') stoneAcc) ss

    let rec runRules (memoMap: MemoMap) (times: int) (stones: (int64 * int64) array) : (int64 * int64) array =
        if times = 0 then
            stones
        else
            let (newMemoMap, newStones) =
                applyRulesToStonesAndUpdateMap memoMap [] (List.ofArray stones)

            runRules newMemoMap (times - 1) newStones

    let beholdStones (blinks: int) (input: string) : string =
        input
        |> parseInput
        |> runRules Map.empty (blinks / memoLength)
        |> Array.map fst
        |> Array.sum
        |> string

    let part1 (input: string) : string = input |> beholdStones 25

    let part2 (input: string) : string = input |> beholdStones 75
