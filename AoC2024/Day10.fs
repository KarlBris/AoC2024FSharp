namespace AoC2024

open Utils
open System

module Day10 =
    type TrailMap = Map<Position, int>

    let parseInput (input: string) : TrailMap =
        input
        |> lines
        |> Array.mapi (fun y line ->
            line
            |> Seq.mapi (fun x c -> if Char.IsNumber c then Some((x, y), charToInt c) else None)
            |> Seq.choose id
            |> Seq.toArray)
        |> Array.concat
        |> Map.ofArray

    let findTrailheads (map: TrailMap) : Position array =
        map |> Map.filter (fun _ v -> v = 0) |> Map.keys |> Seq.toArray

    let rec walkTrail
        (currentHeight: int)
        (positionsWalked: Position list)
        (map: TrailMap)
        (currentPosition: Position)
        : (Position list) array =
        if currentHeight = 9 then
            [| currentPosition :: positionsWalked |]
        else
            let possibleNewPositions =
                manhattanNeighborPositions
                |> Array.map (fst >> (addPos currentPosition))
                |> Array.choose (fun p -> map |> Map.tryFind p |> Option.map (fun x -> (p, x)))
                |> Array.filter (fun (_, h) -> h = currentHeight + 1)
                |> Array.map fst

            possibleNewPositions
            |> Array.map (walkTrail (currentHeight + 1) (currentPosition :: positionsWalked) map)
            |> Array.concat

    let part1 (input: string) : string =
        let map = parseInput input
        let trailheads = findTrailheads map

        let result =
            trailheads
            |> Array.map (fun p -> walkTrail 0 [] map p |> Array.map List.head |> Array.distinct |> Array.length)
            |> Array.sum

        result |> string

    let part2 (input: string) : string =
        let map = parseInput input
        let trailheads = findTrailheads map

        let trails =
            trailheads
            |> Array.map (fun p -> walkTrail 0 [] map p |> Array.distinct |> Array.length)
            |> Array.sum

        trails |> string
