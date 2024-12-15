namespace AoC2024

open Utils

module Day15 =
    type RobotMap = Map<Position, char>
    type MoveList = Direction list

    let expandInput (inputLine: string) : string =
        inputLine
        |> Seq.map (fun c ->
            match c with
            | '#' -> "##"
            | '@' -> "@."
            | 'O' -> "[]"
            | _ -> "..")
        |> String.concat ""

    let parseInput (part2: bool) (input: string) : (RobotMap * MoveList) =
        let (map, ms) = input |> splitAtDoubleLines |> twoArrayToTuple

        let parsedMap =
            map
            |> lines
            |> Array.mapi (fun y line ->
                if part2 then expandInput line else line
                |> Seq.mapi (fun x c -> ((x, y), c))
                |> Seq.filter (fun (_, c) -> c <> '.')
                |> Seq.toArray)
            |> Array.concat
            |> Map.ofArray

        let parsedInstructions =
            ms |> lines |> Seq.concat |> Seq.map directionOfChar |> List.ofSeq

        (parsedMap, parsedInstructions)

    let gpsScore (((x, y), _): Position * char) : int = x + (y * 100)

    let mapGpsScore (map: RobotMap) : int =
        map
        |> Map.filter (fun _ v -> v = 'O' || v = '[')
        |> Map.toArray
        |> Array.map gpsScore
        |> Array.sum

    let rec probeDirection
        (acc: ((char * Position) list) list)
        (map: RobotMap)
        (dir: Direction)
        : (char * Position) list =
        let checkPositions = acc |> List.head |> List.map (snd >> oneStep dir)

        let addToAcc =
            checkPositions
            |> List.map (fun checkPos ->
                match Map.tryFind checkPos map with
                | None -> []
                | Some c ->
                    if c = '[' && (dir = N || dir = S) then
                        [ (c, checkPos); (']', oneStep E checkPos) ]
                    elif c = ']' && (dir = N || dir = S) then
                        [ (c, checkPos); ('[', oneStep W checkPos) ]
                    else
                        [ (c, checkPos) ])
            |> List.concat
            |> List.distinct

        if addToAcc.IsEmpty then List.concat acc
        elif List.exists (fun (c, _) -> c = '#') addToAcc then []
        else probeDirection (addToAcc :: acc) map dir

    let moveRobot (map: RobotMap) (dir: Direction) : RobotMap =
        let robotPos = map |> Map.findKey (fun _ v -> v = '@')

        match probeDirection [ [ '@', robotPos ] ] map dir with
        | [] -> map
        | thingsToMove ->
            let mapWithout = map |> removeKeysFromMap (robotPos :: (List.map snd thingsToMove))
            let newObstacles = thingsToMove |> List.map (fun (oC, oP) -> (oneStep dir oP, oC))
            let newRobot = (oneStep dir robotPos, '@')

            mapWithout |> addKeysToMap (newRobot :: newObstacles)

    let rec runRobot (directions: MoveList) (map: RobotMap) : RobotMap =
        match directions with
        | [] -> map
        | d :: ds -> moveRobot map d |> runRobot ds

    let simulateRobots (input: string) (part2: bool) : string =
        let (map, moves) = input |> (parseInput part2)

        runRobot moves map |> mapGpsScore |> string

    let part1 (input: string) : string = simulateRobots input false

    let part2 (input: string) : string = simulateRobots input true
