namespace AoC2024

open Utils

module Day16 =
    type Maze = Map<Position,char>
    
    let parseInput (input:string) : Maze =
        input
        |> lines
        |> Array.mapi(fun y line ->
            line 
            |> Seq.mapi (fun x c -> ((x,y),c))
            |> Array.ofSeq)
        |> Array.concat
        |> Map.ofArray
    
    let rec findNode (visiteds: Position list) (map: Maze) ((pos,dir): Position*Direction) : Position option = 
        match map |> Map.tryFind pos with
        | Some '#' -> None
        | _ ->
            let availableSteps = 
                manhattanNeighborPositions 
                |> Array.map (fun (p,d) -> ((addPos p pos),d)) 
                |> Array.filter (fun (p,_) -> (map |> Map.tryFind p |> Option.defaultValue '.' <> '#') && (visiteds |> List.contains p |> not))

            if availableSteps.Length = 0 then
                Some pos
            else
                if availableSteps |> Array.exists (fun (_,d) -> d <> dir) then 
                    Some pos 
                else 
                    findNode (pos::visiteds) map (oneStep dir pos, dir)

    let findNodes (map: Maze) : Position array =
        let allPositions = map |> Map.filter (fun _ v -> v <> '#') |> Map.keys |> Array.ofSeq
        allPositions |> Array.filter (fun p ->
            let neighbors = manhattanNeighborPositions |> Array.map (fst >> (addPos p)) |> Array.filter (fun n -> allPositions |> Array.contains n)
            if neighbors.Length = 2 then
                let ((x1,y1),(x2,y2)) = twoArrayToTuple neighbors
                not ((x1=x2) || (y1=y2))
            else
                true
            )

    let findNeighborNodes (directionsToCheck : Direction array) ((px, py): Position) (allNodes: Position array) : (Position * Direction) array =
        // TODO: sortera innan
        let upNode = 
            if directionsToCheck |> Array.contains N then 
                allNodes
                |> Array.filter (fun (x, y) -> y < py && x = px)
                |> Array.sortByDescending (snd)
                |> Array.tryHead
                |> Option.map (fun p -> p,N)
            else None
        let downNode = 
            if directionsToCheck |> Array.contains S then 
                allNodes
                |> Array.filter (fun (x, y) -> y > py && x = px)
                |> Array.sortBy (snd)
                |> Array.tryHead
                |> Option.map (fun p -> p,S)
            else None
        let rightNode = 
            if directionsToCheck |> Array.contains E then 
                allNodes
                |> Array.filter (fun (x, y) -> x > px && y = py)
                |> Array.sortBy (fst)
                |> Array.tryHead
                |> Option.map (fun p -> p,E)
            else None
        let leftNode = 
            if directionsToCheck |> Array.contains W then 
                allNodes
                |> Array.filter (fun (x, y) -> x < px && y = py)
                |> Array.sortByDescending (fst)
                |> Array.tryHead
                |> Option.map (fun p -> p,W)
            else None

        [|upNode;downNode;rightNode;leftNode|] |> Array.choose id

    let checkAllNodes (map:Maze) (allNodes: Position array) : (Position*((Position*Direction) array)) array =
        allNodes 
        |> Array.map (fun n ->
            let directionsToCheck = directions |> Array.filter (fun d -> map |> Map.tryFind (oneStep d n) |> Option.isNone)
            (n,findNeighborNodes directionsToCheck n allNodes))            

    let separateIntoDirections (nodeConnections: (Position*((Position*Direction) array)) array) : (Direction * ((Position*Position) array)) array =
        nodeConnections
        |> Array.collect (fun (fromPos, toNodes) -> toNodes |> Array.map (fun (toNode,d) -> (d,(fromPos,toNode))))
        |> Array.groupBy fst
        |> Array.map (fun (d,x) -> (d, x |> Array.map snd))

    let connectDirections (a: (Position*Direction) array) (b: (Position*Direction) array) : (((Position*Direction)*(Position*Direction))*int) array = 
        let allCommonPoints = a |> Array.filter (fun (pA,_) -> b |> Array.exists (fun (pB,_) -> pA=pB)) |> Array.map fst


        let hej = allCommonPoints |> Array.map (fun p -> (((a |> Array.find (fun (pos,_) -> pos = p)), (b |> Array.find (fun (pos,_) -> pos = p))), 1000))


        hej

    let connectAllDirections (input: (Direction * ((Position*Position) array)) array): (((Position*Direction) * (Position*Direction)) * int) array =
        let placesWhereYouCanFaceNorth = input |> Array.find (fun (d,_) -> d = N) |> snd |> Array.collect (fun (f,a) -> [|(f,N);(a,N)|])
        let placesWhereYouCanFaceSouth = input |> Array.find (fun (d,_) -> d = S) |> snd |> Array.collect (fun (f,a) -> [|(f,S);(a,S)|])
        let placesWhereYouCanFaceEast = input |> Array.find (fun (d,_) -> d = E) |> snd |> Array.collect (fun (f,a) -> [|(f,E);(a,E)|])
        let placesWhereYouCanFaceWest = input |> Array.find (fun (d,_) -> d = W) |> snd |> Array.collect (fun (f,a) -> [|(f,W);(a,W)|])

        let upLeftConnections = connectDirections placesWhereYouCanFaceNorth placesWhereYouCanFaceWest
        let leftDownConnections = connectDirections placesWhereYouCanFaceWest placesWhereYouCanFaceSouth
        let downRightConnections = connectDirections placesWhereYouCanFaceSouth placesWhereYouCanFaceEast
        let rightUpConnections = connectDirections placesWhereYouCanFaceEast placesWhereYouCanFaceNorth 

        let leftUpConnections = connectDirections placesWhereYouCanFaceWest placesWhereYouCanFaceNorth
        let downLeftConnections = connectDirections placesWhereYouCanFaceSouth placesWhereYouCanFaceWest
        let rightDownConnections = connectDirections placesWhereYouCanFaceEast placesWhereYouCanFaceSouth
        let upRightConnections = connectDirections placesWhereYouCanFaceNorth placesWhereYouCanFaceEast

        Array.concat [|upLeftConnections;leftDownConnections;downRightConnections;rightUpConnections;leftUpConnections;downLeftConnections;rightDownConnections;upRightConnections|]

    let setInSortedDistanceMap key value (distanceMap: ((Position*Direction)*int) list) : ((Position*Direction)*int) list=
        
        let mapWithout =
            match distanceMap |> List.tryFindIndex (fun (pd,_) -> pd = key) with
            | Some i -> distanceMap |> List.removeAt i
            | None -> distanceMap
        
        match mapWithout |> List.tryFindIndex (fun (_,v)-> v >= value) with
        | Some i -> mapWithout |> List.insertAt i (key,value)
        | None -> List.append mapWithout [(key,value)]

    let rec dijkstraLoop (index: int) (costs: Map<((Position*Direction)*(Position*Direction)),int>) (distanceMap: ((Position*Direction)*int) list) (previousMap: Map<(Position),(Position) list>) (queue: (Position*Direction) array) (endPos: Position) =
        if (queue |> Array.isEmpty) then
            (distanceMap, previousMap)
        else 
            let (u,distU) = distanceMap[index]
            let newQueue = queue |> Array.filter (fun q -> q <> u)

            let neighborsOfU = costs |> Map.toList |> List.filter (fun ((p,n),_) -> (p = u && newQueue |> Array.contains n)) |> List.map (fun ((_,n),_) -> n)

            if u = ((3,7),E) then
                ()
            
            let (d',p') = 
                neighborsOfU 
                |> List.fold (fun (dMap, pMap) v -> 
                    let alt = distU + costs[(u,v)]
                    if alt <= (distanceMap |> List.find (fun (a,_) -> a = v) |> snd) then 
                        let newDistanceMap = dMap |> setInSortedDistanceMap v alt
                        //let newPrevMap = pMap |> Map.add v u
                        let newPrevMap = pMap |> Map.change (fst v) (fun vertexListOption -> vertexListOption |> Option.map (fun l -> (fst u)::l) |> Option.defaultValue [(fst u)] |> Some )

                        (newDistanceMap,newPrevMap)
                    else 
                        (dMap, pMap)
                
                    ) (distanceMap, previousMap)

            dijkstraLoop (index+1) costs d' p' newQueue endPos

    let rec followPaths (visiteds: Position list) (current: (Position) list) (prevMap: Map<(Position),(Position) list>) : (Position) list =
        let newPositions = current |> List.choose (fun a -> prevMap |> Map.tryFind a) |> List.concat |> List.except visiteds

        let newInbetweens =
            current 
            |> List.choose (fun a -> prevMap |> Map.tryFind a |> Option.map (fun ns -> ns |> List.map ((makeLineBetween a))))
            |> List.concat
            |> List.concat
            |> List.distinct

        if newPositions.IsEmpty then
            []
        else
            List.append newInbetweens (followPaths (List.append newPositions visiteds) newPositions prevMap)

    let dijkstra (nodes: (Position*Direction) array) (costs: (((Position*Direction)*(Position*Direction))*int) array) (startPos: (Position*Direction)) (endPos: Position) : int*int =
        let distMap: Map<(Position*Direction),int> = nodes |> Array.map (fun n -> (n,System.Int32.MaxValue)) |> Map.ofArray
        let prevMap: Map<(Position),(Position) list> = Map.empty
        let costMap = costs |> Map.ofArray

        let distMapStarer = distMap |> Map.change startPos (fun _ -> Some 0) |> Map.toList |> List.sortBy (snd)

        let (dist,prev) = dijkstraLoop 0 costMap distMapStarer prevMap nodes endPos

        let prevPaths = followPaths [endPos] [endPos] (prev |> Map.map (fun _ v -> v |> List.distinct)) |> List.distinct

        (dist |> List.toArray |> Array.filter (fun ((p,_),_) -> p = endPos) |> Array.map (snd) |> Array.min, prevPaths.Length)

    let doThing (things: (Direction*((Position*Position)*int)array)array) =
        things
        |> Array.collect(fun (d,rest) -> rest |> Array.map (fun ((p1,p2),cost) -> ((p1,d),(p2,d)),cost))

    let part1 (input: string) : string = 
        let foo = input |> parseInput

        let goalPos = foo |> Map.findKey (fun _ v -> v = 'E')
        let startPos = foo |> Map.findKey (fun _ v -> v = 'S')

        let fixedMap = foo |> Map.remove goalPos |> Map.remove startPos |> Map.filter (fun _ v -> v <> '.')

        let allNodes = findNodes foo 
        let nodeConnections = checkAllNodes fixedMap allNodes 

        let separateDirectionNodeConnections = separateIntoDirections nodeConnections
        let calculatedCosts = separateDirectionNodeConnections |> Array.map (fun (d,cs) -> d, cs |> Array.map (fun (a,b) ->(a,b), manhattanDistance a b))
        let allNormalCosts = doThing calculatedCosts
        let interDirectionCosts = separateDirectionNodeConnections |> connectAllDirections

        let totalCosts = Array.concat [|allNormalCosts;interDirectionCosts;[|(((startPos,E),(startPos,N)),1000)|]|] |> Array.distinct
        //let allDirectedNodes = totalCosts |> Array.collect (fun ((pd1,pd2),_) -> [|pd1;pd2|]) |> Array.distinct
        let allDirectedNodes = separateDirectionNodeConnections |> Array.collect (fun (d,ppa) -> ppa |> Array.map (fun (p1,p2) -> [|(p1,d);(p2,d)|])) |> Array.concat |> Array.append [|(startPos,E)|] |> Array.distinct

        let mnl = allNodes.Length

        //let (nodes, costs) = chartMaze [] [] fixedMap (startPos, E)

        let chartMaze = addKeysToMap (allNodes |> Array.map (fun p -> (p,'+'))) fixedMap
        //printMapAutoBounds chartMaze

        let result = dijkstra allDirectedNodes totalCosts (startPos,E) goalPos
        //let result = traverseMap 0 [] 0 goalPos fixedMap (startPos, E)


        result |> fst |> string

    let part2 (input: string) : string = 
        let foo = input |> parseInput

        let goalPos = foo |> Map.findKey (fun _ v -> v = 'E')
        let startPos = foo |> Map.findKey (fun _ v -> v = 'S')

        let fixedMap = foo |> Map.remove goalPos |> Map.remove startPos |> Map.filter (fun _ v -> v <> '.')

        let allNodes = findNodes foo 
        let nodeConnections = checkAllNodes fixedMap allNodes 

        let separateDirectionNodeConnections = separateIntoDirections nodeConnections
        let calculatedCosts = separateDirectionNodeConnections |> Array.map (fun (d,cs) -> d, cs |> Array.map (fun (a,b) ->(a,b), manhattanDistance a b))
        let allNormalCosts = doThing calculatedCosts
        let interDirectionCosts = separateDirectionNodeConnections |> connectAllDirections

        let totalCosts = Array.concat [|allNormalCosts;interDirectionCosts;[|(((startPos,E),(startPos,N)),1000)|]|] |> Array.distinct
        //let allDirectedNodes = totalCosts |> Array.collect (fun ((pd1,pd2),_) -> [|pd1;pd2|]) |> Array.distinct
        let allDirectedNodes = separateDirectionNodeConnections |> Array.collect (fun (d,ppa) -> ppa |> Array.map (fun (p1,p2) -> [|(p1,d);(p2,d)|])) |> Array.concat |> Array.append [|(startPos,E)|] |> Array.distinct

        let mnl = allNodes.Length

        //let (nodes, costs) = chartMaze [] [] fixedMap (startPos, E)

        let chartMaze = addKeysToMap (allNodes |> Array.map (fun p -> (p,'+'))) fixedMap
        //printMapAutoBounds chartMaze

        let result = dijkstra allDirectedNodes totalCosts (startPos,E) goalPos
        //let result = traverseMap 0 [] 0 goalPos fixedMap (startPos, E)


        result |> snd |> string
