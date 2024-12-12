namespace AoC2024

open Utils

module Day12 =
    type PlotMap = Map<Position, char>

    let parseInput (input: string) : PlotMap =
        input
        |> lines
        |> Array.mapi (fun y line -> line |> Seq.mapi (fun x c -> ((x, y), c)) |> Array.ofSeq)
        |> Array.concat
        |> Map.ofArray

    let neighborPositions (pos: Position) : Position list =
        manhattanNeighborPositions |> Array.map (fun (pD, _) -> addPos pos pD) |> Array.toList

    let rec floodRegion (acc: Position list) (potentialRegion: PlotMap) (currentPositions: Position list) : Position list =
        let filteredNeighborPositions =
            currentPositions
            |> List.collect neighborPositions
            |> List.distinct
            |> List.filter (fun p -> potentialRegion.Keys.Contains p && (not (List.contains p acc)))

        if filteredNeighborPositions.IsEmpty then
            List.append currentPositions acc
        else
            let newAcc = List.append currentPositions acc
            floodRegion newAcc potentialRegion filteredNeighborPositions

    let rec findRegions (potentialRegion: PlotMap) : PlotMap list =
        if potentialRegion.IsEmpty then
            []
        else
            let region =
                potentialRegion |> Map.keys |> Seq.head |> (fun p -> floodRegion [] potentialRegion [ p ])

            let newPlotMap =
                potentialRegion |> Map.filter (fun k t -> List.contains k region |> not)

            (region |> List.map (fun p -> (p, potentialRegion.Values |> Seq.head)) |> Map.ofList)
            :: (findRegions newPlotMap)

    let separateRegions (allPlots: PlotMap) : Position array array =
        allPlots
        |> Map.toArray
        |> Array.groupBy (fun (_, v) -> v)
        |> Array.map (snd >> Map.ofArray)
        |> Array.map findRegions
        |> List.concat
        |> Array.ofList
        |> Array.map (Map.keys >> Array.ofSeq)

    let area (region: Position array) : int = region |> Array.length

    let perimeter (region: Position array) : int =
        region
        |> Array.map (fun p ->
            let neighbors = neighborPositions p
            neighbors |> Seq.filter (fun nP -> (Array.contains nP region) |> not) |> Seq.length)
        |> Seq.sum

    let calculatePrice (input: string) (factorFunction: Position array -> int) : string =
        input
        |> parseInput
        |> separateRegions
        |> Array.map (fun r -> (area r) * (factorFunction r))
        |> Array.sum
        |> string

    let part1 (input: string) : string = calculatePrice input perimeter

    let isFree (region: Position array) (dir: Direction) (pos: Position) : bool =
        manhattanNeighborPositions
        |> Array.find (fun (_, d) -> d = dir)
        |> (fun (pD, _) -> Array.contains (addPos pos pD) region |> not)

    let getFreeSides (region: Position array) : (Position array) array =
        [| N; S; E; W |]
        |> Array.map (fun dir ->
            region
            |> Array.choose (fun pos ->
                if isFree region dir pos then
                    Some pos
                else
                    None))

    let countContiguous (regions: Position array) : int =
        regions |> Array.map (fun p -> (p, 'A')) |> Map.ofArray |> findRegions |> List.length

    let sides (region: Position array) : int =
        let freeSides = getFreeSides region
        freeSides |> Array.map countContiguous |> Array.sum

    let part2 (input: string) : string = calculatePrice input sides
