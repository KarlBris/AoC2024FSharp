namespace AoC2024

open Utils

module Day02 =

    let sequentialCheck (checkfun: (int * int) -> bool) (report: int array) : bool =
        report
        |> Array.windowed 2
        |> Array.map twoArrayToTuple
        |> Array.forall checkfun

    let isSafeReport (report: int array) : bool =
        (sequentialCheck (fun (a, b) -> a < b) report
         || sequentialCheck (fun (a, b) -> a > b) report)
        && (sequentialCheck (fun (a, b) -> abs (a - b) < 4) report)

    let part1 (input: string) : string =
        input
        |> lines
        |> Array.map words
        |> Array.map (Array.map int)
        |> Array.filter isSafeReport
        |> Array.length
        |> string

    let reportVersions (report: int array) : int array array =
        report
        |> Array.mapi (fun i _ -> report |> Array.removeAt i)

    let part2 (input: string) : string =
        input
        |> lines
        |> Array.map words
        |> Array.map (Array.map int)
        |> Array.map reportVersions
        |> Array.filter (Array.exists isSafeReport)
        |> Array.length
        |> string
