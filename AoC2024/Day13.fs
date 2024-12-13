namespace AoC2024

open Utils

module Day13 =
    type ClawMachine = (int64 * int64) * (int64 * int64) * (int64 * int64)

    let parseRow (skipLength: int) (buttonString: string) : (int64 * int64) =
        buttonString.Substring(skipLength)
        |> commas
        |> Array.map (fun s -> s.Substring(2) |> int64)
        |> twoArrayToTuple

    let parseInput (input: string) : ClawMachine array =
        input
        |> splitAtDoubleLines
        |> Array.map (fun machine ->
            let mLine = machine |> lines
            (parseRow 10 mLine[0], parseRow 10 mLine[1], parseRow 7 mLine[2]))

    let solveMachine (((aX, aY), (bX, bY), (pX, pY)): ClawMachine) : int64 option =
        let aT = ((bY * pX) - (bX * pY)) / ((bY * aX) - (bX * aY))
        let bT = ((aX * pY) - (aY * pX)) / ((bY * aX) - (bX * aY))

        if (((aT * aX) + (bT * bX)) = pX) && (((aT * aY) + (bT * bY)) = pY) then
            Some((aT * 3L) + bT)
        else
            None

    let playClaw (playFun: ClawMachine -> int64 option) (input: string) : string =
        input |> parseInput |> Array.choose playFun |> Array.sum |> string

    let part1 (input: string) : string = playClaw solveMachine input

    let addCoords ((a, b, (pX, pY)): ClawMachine) : ClawMachine =
        let additional = 10000000000000L
        (a, b, (pX + additional, pY + additional))

    let part2 (input: string) : string =
        playClaw (addCoords >> solveMachine) input
