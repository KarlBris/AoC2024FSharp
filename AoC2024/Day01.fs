namespace AoC2024

open Utils
open System

module Day01 =

    let splitInput (input: string) : int array * int array =
        input
        |> lines
        |> Array.map words
        |> Array.map twoArrayToTuple
        |> Array.map (fun (a, b) -> (int a, int b))
        |> Array.unzip

    let part1 (input: string) : string =
        input
        |> splitInput
        |> (fun (l, r) -> Array.fold2 (fun s l' r' -> s + (abs (l' - r'))) 0 (Array.sort l) (Array.sort r))
        |> string

    let part2 (input: string) : string =
        let (l, r) = input |> splitInput

        let rMap = r |> Array.countBy id |> Map.ofArray

        l
        |> Array.fold
            (fun s n ->
                s + (n * ((Map.tryFind n rMap) |> Option.defaultValue 0)))
            0
        |> string
