namespace AoC2024

open Utils

module Day08 =

    let parseInput (input: string) : int * ((char * (Position array)) array) =
        let linesInput = input |> lines

        (linesInput.Length,
         linesInput
         |> Array.mapi (fun y line ->
             line
             |> Seq.mapi (fun x char -> if char <> '.' then Some(char, (x, y)) else None)
             |> Seq.choose id
             |> Seq.toArray)
         |> Array.concat
         |> Array.groupBy fst
         |> (fun a -> a)
         |> Array.map (fun (k, vs) -> (k, vs |> Array.map snd)))

    let makeAntinodesForPair (part2: bool) (mapSize: int) (((x1, y1), (x2, y2)): Position * Position) : Position array =
        let xDiff = x1 - x2
        let yDiff = y1 - y2

        let firstDir =
            numbersFrom 1
            |> Seq.map (fun n -> (x1 + (n * xDiff), y1 + (n * yDiff)))
            |> Seq.takeWhile (isValidPosition (mapSize - 1, mapSize - 1))
            |> Array.ofSeq

        let secondDir =
            numbersFrom 1
            |> Seq.map (fun n -> (x2 - (n * xDiff), y2 - (n * yDiff)))
            |> Seq.takeWhile (isValidPosition (mapSize - 1, mapSize - 1))
            |> Array.ofSeq

        let firstToUse =
            if part2 then
                Array.append [| x1, y1 |] firstDir
            else
                Seq.tryHead firstDir |> Option.map Array.singleton |> Option.defaultValue [||]

        let secondToUse =
            if part2 then
                Array.append [| x2, y2 |] secondDir
            else
                Seq.tryHead secondDir |> Option.map Array.singleton |> Option.defaultValue [||]

        Array.append firstToUse secondToUse

    let makeAntinodes (part2: bool) (mapSize: int) (collection: Position array) : Position array =
        collection
        |> List.ofArray
        |> makePairs
        |> List.map (makeAntinodesForPair part2 mapSize)
        |> Array.concat

    let findAntinodes (part2: bool) ((mapSize, map): int * ((char * (Position array)) array)) : Position array =
        map
        |> Array.map (fun (_, vs) -> makeAntinodes part2 mapSize vs)
        |> Array.concat
        |> Array.distinct

    let part1 (input: string) : string =
        input |> parseInput |> findAntinodes false |> Array.length |> string

    let part2 (input: string) : string =
        input |> parseInput |> findAntinodes true |> Array.length |> string
