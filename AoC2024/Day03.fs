namespace AoC2024

open Utils
open System.Text.RegularExpressions

module Day03 =
    let parseInput (input: string) : (int * int) array =
        Regex.Matches(input, "mul\(\d+,\d+\)")
        |> Seq.cast<Match>
        |> Seq.map (fun mtch -> mtch.Value[4 .. (mtch.Length - 2)])
        |> Array.ofSeq
        |> Array.map (commas >> twoArrayToTuple)
        |> Array.map (fun (a, b) -> (int a, int b))

    let rec filterMuls (isDo: bool) (acc: int) (matches: Match list) : int =
        match matches with
        | [] -> acc
        | (m :: ms) ->
            if m.Groups[1].Length <> 0 then
                filterMuls false acc ms
            elif m.Groups[2].Length <> 0 then
                filterMuls true acc ms
            else
                let acc' =
                    if isDo then
                        let (a, b) =
                            m.Groups[3].Value[4 .. (m.Groups[3].Length - 2)]
                            |> (commas >> twoArrayToTuple)

                        (acc + ((int a) * (int b)))
                    else
                        acc

                filterMuls isDo acc' ms

    let parseDoInput (input: string) : int =
        Regex.Matches(input, "(don't\(\))|(do\(\))|(mul\(\d+,\d+\))")
        |> Seq.cast<Match>
        |> Seq.toList
        |> filterMuls true 0

    let part1 (input: string) : string =
        input
        |> parseInput
        |> Array.map (fun (a, b) -> a * b)
        |> Array.sum
        |> string

    let part2 (input: string) : string = input |> parseDoInput |> string
