namespace AoC2024

open Utils

module Day06 =
    let parseInput (input: string) : (Position array * (Position * Direction) * int) =
        let inputLines = input |> lines

        let (guardPos, obstacles) =
            inputLines
            |> Array.mapi (fun x line ->
                line
                |> Seq.mapi (fun y ch ->
                    if ch = '#' then Some(false, x, y)
                    elif ch = '^' then Some(true, x, y)
                    else None)
                |> Seq.choose id
                |> Seq.toArray)
            |> Array.concat
            |> Array.partition (fun (a, _, _) -> a)

        (obstacles |> Array.map (fun (_, x, y) -> (x, y)),
         ((guardPos
           |> Array.exactlyOne
           |> (fun (_, x, y) -> (x, y))),
          N),
         inputLines.Length)

    let findNearestObstacle (((px, py), dir): (Position * Direction)) (obstacles: Position array) : Position option =
        match dir with
        | N -> // -x
            obstacles
            |> Array.filter (fun (x, y) -> x < px && y = py)
            |> Array.sortByDescending (fst)
            |> Array.tryHead
        | S -> // +x
            obstacles
            |> Array.filter (fun (x, y) -> x > px && y = py)
            |> Array.sortBy (fst)
            |> Array.tryHead
        | E -> // +y
            obstacles
            |> Array.filter (fun (x, y) -> y > py && x = px)
            |> Array.sortBy (snd)
            |> Array.tryHead
        | W -> // -y
            obstacles
            |> Array.filter (fun (x, y) -> y < py && x = px)
            |> Array.sortByDescending (snd)
            |> Array.tryHead

    let findEdgePos (size: int) (((x, y), dir): (Position * Direction)) : Position =
        match dir with
        | N -> (0, y)
        | S -> (size - 1, y)
        | E -> (x, size - 1)
        | W -> (x, 0)

    let oneStep (dir: Direction) ((x, y): Position) : Position =
        match dir with
        | N -> (x - 1, y)
        | E -> (x, y + 1)
        | S -> (x + 1, y)
        | W -> (x, y - 1)

    let rec walkCycle
        (size: int)
        (startPositions: (Position * Direction) list)
        ((pos, dir): (Position * Direction))
        (obstacles: Position array)
        (visiteds: Position list list)
        : (Position list list) option =
        let nearestObstacle = findNearestObstacle (pos, dir) obstacles

        if startPositions |> List.contains (pos, dir) then
            None
        else
            match nearestObstacle with
            | None ->
                let edgePos = findEdgePos size (pos, dir)
                let inbetweens = makeLineBetween pos edgePos
                Some(inbetweens :: visiteds)
            | Some oPos ->
                let newPos = oneStep (directionReverse dir) oPos
                let newDir = turn dir R
                let inbetweens = makeLineBetween pos newPos
                walkCycle size ((pos, dir) :: startPositions) (newPos, newDir) obstacles (inbetweens :: visiteds)

    let part1 (input: string) : string =
        let (obstacles, guardPos, mapSize) = input |> parseInput

        walkCycle mapSize [] guardPos obstacles []
        |> Option.get
        |> List.map (Set.ofList)
        |> Set.ofList
        |> Set.unionMany
        |> Set.count
        |> string

    let rec placeObstacles
        (size: int)
        (p: (Position * Direction))
        (obstacles: Position array)
        (acc: int)
        (potentialObstacles: Position list)
        : int =
        match potentialObstacles with
        | [] -> acc
        | o :: os ->
            let guardPath = walkCycle size [] p (Array.append [| o |] obstacles) []

            match guardPath with
            | None -> placeObstacles size p obstacles (acc + 1) os
            | Some _ -> placeObstacles size p obstacles (acc) os

    let part2 (input: string) : string =
        let (obstacles, guardPos, mapSize) = input |> parseInput

        walkCycle mapSize [] guardPos obstacles []
        |> Option.get
        |> List.map (List.rev)
        |> List.concat
        |> List.distinct
        |> placeObstacles mapSize guardPos obstacles 0
        |> string
