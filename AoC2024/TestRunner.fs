﻿namespace AoC2024

open System.IO
open System.Diagnostics
open System

module TestRunner =

    let getInput day =
        let filename = $"..\..\..\inputs\input_day{day}.txt"

        if File.Exists filename then
            filename
            |> File.ReadAllText
            |> String.filter (fun c -> c <> '\r')
        else
            failwith $"Input file {filename} not found"

    let makeComparison (expectedResults: string []) (results: string []) =
        Array.zip results expectedResults
        |> Array.map (fun (r, e) -> (r, e, r = e))

    let printStatus ((res, expectedRes, success): string * string * bool) =
        printfn "%s! Got %s, expected %s." (if success then "Success" else "Failure") res expectedRes

    let formatTime (span: TimeSpan) : string =
        $"{span.Hours} Hours, {span.Minutes} Minutes, {span.Seconds} Seconds, {span.Milliseconds}.{span.Microseconds} Milliseconds"

    let run (examples: string []) expectedResults realInput (func: string -> string) title =
        printfn title

        if examples.Length = 0 then
            printfn "No examples found, running the real input..."
        else
            printfn "Running and verifying examples before the real input..."

        let resultList =
            examples
            |> Array.map func
            |> makeComparison expectedResults

        resultList |> Array.map printStatus |> ignore

        let examplesSuccessful =
            resultList
            |> Array.fold (fun b1 (_, _, b2) -> b1 && b2) true

        if examplesSuccessful then
            printfn "All examples were successful, running the real input..."
            let timer = new Stopwatch()
            timer.Start()
            printfn "Result from real input: %s" (func realInput)
            timer.Stop()
            printfn $"Time elapsed: {(formatTime timer.Elapsed)}"
            printfn "Time elapsed: %A" timer.Elapsed
        else
            printfn "Some examples were not successful. PLEASE DO BETTER"

        printfn ""

    // Day1
    let input1 = getInput 1

    let examples1_1 = [| "3   4\n4   3\n2   5\n1   3\n3   9\n3   3" |]

    let exampleResults1_1 = [| "11" |]

    let examples1_2 = examples1_1

    let exampleResults1_2 = [| "31" |]

    // Day2
    let input2 = getInput 2

    let examples2_1 = [| "7 6 4 2 1\n1 2 7 8 9\n9 7 6 2 1\n1 3 2 4 5\n8 6 4 4 1\n1 3 6 7 9" |]

    let exampleResults2_1 = [| "2" |]

    let examples2_2 = examples2_1

    let exampleResults2_2 = [| "4" |]

    // Day3
    let input3 = getInput 3

    let examples3_1 = [| "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))" |]

    let exampleResults3_1 = [| "161" |]

    let examples3_2 = [| "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))" |]

    let exampleResults3_2 = [| "48" |]
