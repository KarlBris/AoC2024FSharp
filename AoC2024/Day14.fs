namespace AoC2024

open Utils

module Day14 =
    type Robot = Position * Position

    let parseInput (input: string) : Robot array =
        input
        |> lines
        |> Array.map (fun line ->
            line
            |> words
            |> Array.map (fun word -> word.Substring(2) |> commas |> Array.map int |> twoArrayToTuple)
            |> twoArrayToTuple)

    let moveRobot ((width, height): int * int) ((pos, vel): Robot) =
        let (newX, newY) = pos |> addPos vel
        let wrappedPos = (eMod newX width, eMod newY height)
        (wrappedPos, vel)

    let rec elapseTime (seconds: int) (size: Position) (robots: Robot array) : Robot array =
        if seconds = 0 then
            robots
        else
            let movedRobots = robots |> Array.map (moveRobot size)
            elapseTime (seconds - 1) size movedRobots

    let quadrant ((width, height): Position) ((x, y): Position) : Position option =
        let a = x - ((width - 1) / 2)
        let b = y - ((height - 1) / 2)

        let newX =
            if a < 0 then Some -1
            elif a > 0 then Some 1
            else None

        let newY =
            if b < 0 then Some -1
            elif b > 0 then Some 1
            else None

        match (newX, newY) with
        | (Some xVal, Some yVal) -> Some(xVal, yVal)
        | _ -> None

    let part1 (input: string) : string =
        let robots = parseInput input
        let size = if robots.Length < 20 then (11, 7) else (101, 103)

        robots
        |> elapseTime 100 size
        |> Array.map fst
        |> Array.choose (quadrant size)
        |> Array.groupBy id
        |> Array.map snd
        |> Array.map (_.Length)
        |> Array.fold (*) 1
        |> string

    let isNeighbors ((aX, aY): Position) ((bX, bY): Position) : bool =
        abs (aX - bX) <= 1 && abs (aY - bY) <= 1

    let rec degreeOfTogetherness (robots: Robot list) : int =
        match robots with
        | [] -> 0
        | (r, _) :: rs ->
            let rTogetherness =
                rs |> List.map (fun (r', _) -> if isNeighbors r r' then 1 else 0) |> List.sum

            rTogetherness + (degreeOfTogetherness rs)

    let rec findChristmasTree (secondsElapsed: int) (size: Position) (robots: Robot array) =
        let movedRobots = robots |> elapseTime 1 size

        let degree = degreeOfTogetherness (List.ofArray movedRobots)

        if degree > 300 then
            secondsElapsed
        else
            findChristmasTree (secondsElapsed + 1) size movedRobots

    let part2 (input: string) : string =
        let robots = parseInput input
        let size = if robots.Length < 20 then (11, 7) else (101, 103)

        findChristmasTree 1 size robots |> string
