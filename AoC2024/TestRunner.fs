namespace AoC2024

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
    let input1() = getInput 1

    let examples1_1 = [| "3   4\n4   3\n2   5\n1   3\n3   9\n3   3" |]

    let exampleResults1_1 = [| "11" |]

    let examples1_2 = examples1_1

    let exampleResults1_2 = [| "31" |]

    // Day2
    let input2() = getInput 2

    let examples2_1 = [| "7 6 4 2 1\n1 2 7 8 9\n9 7 6 2 1\n1 3 2 4 5\n8 6 4 4 1\n1 3 6 7 9" |]

    let exampleResults2_1 = [| "2" |]

    let examples2_2 = examples2_1

    let exampleResults2_2 = [| "4" |]

    // Day3
    let input3() = getInput 3

    let examples3_1 = [| "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))" |]

    let exampleResults3_1 = [| "161" |]

    let examples3_2 = [| "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))" |]

    let exampleResults3_2 = [| "48" |]

    // Day4
    let input4() = getInput 4

    let examples4_1 = [| "MMMSXXMASM\nMSAMXMSMSA\nAMXSXMAAMM\nMSAMASMSMX\nXMASAMXAMM\nXXAMMXXAMA\nSMSMSASXSS\nSAXAMASAAA\nMAMMMXMMMM\nMXMXAXMASX" |]

    let exampleResults4_1 = [| "18" |]

    let examples4_2 = examples4_1

    let exampleResults4_2 = [| "9" |]

    // Day5
    let input5() = getInput 5

    let examples5_1 = [| "47|53\n97|13\n97|61\n97|47\n75|29\n61|13\n75|53\n29|13\n97|29\n53|29\n61|53\n97|53\n61|29\n47|13\n75|47\n97|75\n47|61\n75|61\n47|29\n75|13\n53|13\n\n75,47,61,53,29\n97,61,53,29,13\n75,29,13\n75,97,47,61,53\n61,13,29\n97,13,75,29,47" |]

    let exampleResults5_1 = [| "143" |]

    let examples5_2 = examples5_1

    let exampleResults5_2 = [| "123" |]

    // Day6
    let input6() = getInput 6

    let examples6_1 = [| "....#.....\n.........#\n..........\n..#.......\n.......#..\n..........\n.#..^.....\n........#.\n#.........\n......#..." |]

    let exampleResults6_1 = [| "41" |]

    let examples6_2 = examples6_1

    let exampleResults6_2 = [| "6" |]

    // Day7
    let input7() = getInput 7

    let examples7_1 = [| "190: 10 19\n3267: 81 40 27\n83: 17 5\n156: 15 6\n7290: 6 8 6 15\n161011: 16 10 13\n192: 17 8 14\n21037: 9 7 18 13\n292: 11 6 16 20" |]

    let exampleResults7_1 = [| "3749" |]

    let examples7_2 = examples7_1

    let exampleResults7_2 = [| "11387" |]

    // Day8
    let input8() = getInput 8

    let examples8_1 = [| "............\n........0...\n.....0......\n.......0....\n....0.......\n......A.....\n............\n............\n........A...\n.........A..\n............\n............" |]

    let exampleResults8_1 = [| "14" |]

    let examples8_2 = examples8_1

    let exampleResults8_2 = [| "34" |]

    // Day9
    let input9() = getInput 9

    let examples9_1 = [| "2333133121414131402" |]

    let exampleResults9_1 = [| "1928" |]

    let examples9_2 = examples9_1

    let exampleResults9_2 = [| "2858" |]

    // Day10
    let input10() = getInput 10

    let examples10_1 = [| "0123\n1234\n8765\n9876";"...0...\n...1...\n...2...\n6543456\n7.....7\n8.....8\n9.....9";"..90..9\n...1.98\n...2..7\n6543456\n765.987\n876....\n987....";"10..9..\n2...8..\n3...7..\n4567654\n...8..3\n...9..2\n.....01";"89010123\n78121874\n87430965\n96549874\n45678903\n32019012\n01329801\n10456732" |]

    let exampleResults10_1 = [| "1";"2";"4";"3";"36" |]

    let examples10_2 = [| ".....0.\n..4321.\n..5..2.\n..6543.\n..7..4.\n..8765.\n..9....";"..90..9\n...1.98\n...2..7\n6543456\n765.987\n876....\n987....";"012345\n123456\n234567\n345678\n4.6789\n56789.";"89010123\n78121874\n87430965\n96549874\n45678903\n32019012\n01329801\n10456732" |]

    let exampleResults10_2 = [| "3";"13";"227";"81" |]

    // Day11
    let input11() = getInput 11

    let examples11_1 = [| "125 17" |]

    let exampleResults11_1 = [| "55312" |]

    let examples11_2 = [|  |]

    let exampleResults11_2 = [|  |]

    // Day12
    let input12() = getInput 12

    let examples12_1 = [| "AAAA\nBBCD\nBBCC\nEEEC";"OOOOO\nOXOXO\nOOOOO\nOXOXO\nOOOOO";"RRRRIICCFF\nRRRRIICCCF\nVVRRRCCFFF\nVVRCCCJFFF\nVVVVCJJCFE\nVVIVCCJJEE\nVVIIICJJEE\nMIIIIIJJEE\nMIIISIJEEE\nMMMISSJEEE" |]

    let exampleResults12_1 = [| "140";"772";"1930" |]

    let examples12_2 = [| "AAAA\nBBCD\nBBCC\nEEEC" ;"OOOOO\nOXOXO\nOOOOO\nOXOXO\nOOOOO";"EEEEE\nEXXXX\nEEEEE\nEXXXX\nEEEEE";"AAAAAA\nAAABBA\nAAABBA\nABBAAA\nABBAAA\nAAAAAA";"RRRRIICCFF\nRRRRIICCCF\nVVRRRCCFFF\nVVRCCCJFFF\nVVVVCJJCFE\nVVIVCCJJEE\nVVIIICJJEE\nMIIIIIJJEE\nMIIISIJEEE\nMMMISSJEEE"|]

    let exampleResults12_2 = [| "80" ;"436";"236";"368";"1206"|]

    // Day13
    let input13() = getInput 13

    let examples13_1 = [| "" |]

    let exampleResults13_1 = [| "" |]

    let examples13_2 = [| "" |]

    let exampleResults13_2 = [| "" |]

    // Day14
    let input14() = getInput 14

    let examples14_1 = [| "" |]

    let exampleResults14_1 = [| "" |]

    let examples14_2 = [| "" |]

    let exampleResults14_2 = [| "" |]

    // Day15
    let input15() = getInput 15

    let examples15_1 = [| "" |]

    let exampleResults15_1 = [| "" |]

    let examples15_2 = [| "" |]

    let exampleResults15_2 = [| "" |]

    // Day16
    let input16() = getInput 16

    let examples16_1 = [| "" |]

    let exampleResults16_1 = [| "" |]

    let examples16_2 = [| "" |]

    let exampleResults16_2 = [| "" |]

    // Day17
    let input17() = getInput 17

    let examples17_1 = [| "" |]

    let exampleResults17_1 = [| "" |]

    let examples17_2 = [| "" |]

    let exampleResults17_2 = [| "" |]

    // Day18
    let input18() = getInput 18

    let examples18_1 = [| "" |]

    let exampleResults18_1 = [| "" |]

    let examples18_2 = [| "" |]

    let exampleResults18_2 = [| "" |]

    // Day19
    let input19() = getInput 19

    let examples19_1 = [| "" |]

    let exampleResults19_1 = [| "" |]

    let examples19_2 = [| "" |]

    let exampleResults19_2 = [| "" |]

    // Day20
    let input20() = getInput 20

    let examples20_1 = [| "" |]

    let exampleResults20_1 = [| "" |]

    let examples20_2 = [| "" |]

    let exampleResults20_2 = [| "" |]

    // Day21
    let input21() = getInput 21

    let examples21_1 = [| "" |]

    let exampleResults21_1 = [| "" |]

    let examples21_2 = [| "" |]

    let exampleResults21_2 = [| "" |]

    // Day22
    let input22() = getInput 22

    let examples22_1 = [| "" |]

    let exampleResults22_1 = [| "" |]

    let examples22_2 = [| "" |]

    let exampleResults22_2 = [| "" |]

    // Day23
    let input23() = getInput 23

    let examples23_1 = [| "" |]

    let exampleResults23_1 = [| "" |]

    let examples23_2 = [| "" |]

    let exampleResults23_2 = [| "" |]

    // Day24
    let input24() = getInput 24

    let examples24_1 = [| "" |]

    let exampleResults24_1 = [| "" |]

    let examples24_2 = [| "" |]

    let exampleResults24_2 = [| "" |]

    // Day25
    let input25() = getInput 25

    let examples25_1 = [| "" |]

    let exampleResults25_1 = [| "" |]

    let examples25_2 = [| "" |]

    let exampleResults25_2 = [| "" |]