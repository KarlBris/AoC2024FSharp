namespace AoC2024

open Utils

module Day09 =
    type Block =
        | Space
        | File of int

    let parseInput (input: string) : (int * Block) array =
        input
        |> Seq.map charToInt
        |> Seq.toArray
        |> Array.mapi (fun i n -> (n, if i % 2 = 0 then File(i / 2) else Space))

    let isSpace ((_, block): (int * Block)) : bool =
        match block with
        | Space -> true
        | File _ -> false

    let containsEmptySpace (disk: (int * Block) array) : bool = disk |> Array.exists isSpace

    let rec trim (disk: (int * Block) array) : (int * Block) array =
        if disk |> Array.isEmpty then
            [||]
        else
            let (_, block) = Array.last disk

            match block with
            | Space -> disk |> removeLastElement |> trim
            | File _ -> disk

    let reallocateBlock (disk: (int * Block) array) : (int * Block) array =
        let (lastBlockSize, File id) = Array.last disk
        let firstSpaceIndex = Array.findIndex isSpace disk
        let (firstSpaceSize, _) = disk[firstSpaceIndex]

        let updatedDisk =
            if lastBlockSize < firstSpaceSize then
                disk
                |> Array.updateAt firstSpaceIndex (lastBlockSize, File id)
                |> Array.insertAt (firstSpaceIndex + 1) (firstSpaceSize - lastBlockSize, Space)
                |> removeLastElement
            elif lastBlockSize > firstSpaceSize then
                disk
                |> Array.updateAt firstSpaceIndex (firstSpaceSize, File id)
                |> Array.updateAt (disk.Length - 1) (lastBlockSize - firstSpaceSize, File id)
            else
                disk
                |> Array.updateAt firstSpaceIndex (lastBlockSize, File id)
                |> removeLastElement

        updatedDisk |> trim

    let rec reallocateBlocks (disk: (int * Block) array) : (int * Block) array =
        if containsEmptySpace disk then
            disk |> reallocateBlock |> reallocateBlocks
        else
            disk

    let computeChecksum (disk: (int * Block) array) : int64 =
        disk
        |> Array.fold
            (fun (sum, startIndex) (size, block) ->
                match block with
                | Space -> (sum, startIndex + size)
                | File id ->
                    let indices = numbersFrom startIndex |> Seq.take size |> Array.ofSeq
                    let blockSum = indices |> Array.fold (fun s n -> s + ((int64 n) * (int64 id))) 0L
                    (sum + blockSum, startIndex + size))
            (0L, 0)
        |> fst

    let part1 (input: string) : string =
        input |> parseInput |> trim |> reallocateBlocks |> computeChecksum |> string

    let rec reallocateBlockSmarter
        (blocksInReverserOrder: (int * (int * Block)) list)
        (disk: (int * Block) array)
        : (int * Block) array =
        match blocksInReverserOrder with
        | [] -> disk
        | (blockIndex, (lastBlockSize, File id)) :: bs ->

            let firstSpaceThatFits =
                Array.tryFindIndex
                    (fun (spaceSize, blockType) ->
                        match blockType with
                        | File _ -> false
                        | Space -> spaceSize >= lastBlockSize)
                    disk

            let (disk', reverseblocks) =
                match firstSpaceThatFits with
                | Some firstSpaceIndex ->
                    if firstSpaceIndex < blockIndex then
                        let (firstSpaceSize, _) = disk[firstSpaceIndex]

                        if lastBlockSize = firstSpaceSize then
                            (disk
                             |> Array.updateAt firstSpaceIndex (lastBlockSize, File id)
                             |> Array.updateAt blockIndex (lastBlockSize, Space)
                             |> trim,
                             bs)
                        else
                            (disk
                             |> Array.updateAt firstSpaceIndex (lastBlockSize, File id)
                             |> Array.insertAt (firstSpaceIndex + 1) (firstSpaceSize - lastBlockSize, Space)
                             |> Array.updateAt (blockIndex + 1) (lastBlockSize, Space)
                             |> trim,
                             bs
                             |> List.map (fun (i, v) -> if i > firstSpaceIndex then (i + 1, v) else (i, v)))
                    else
                        (disk, bs)
                | None -> (disk, bs)

            reallocateBlockSmarter reverseblocks disk'

    let part2 (input: string) : string =
        let disk = input |> parseInput |> trim

        let blocksInReverserOrder =
            disk
            |> Array.indexed
            |> Array.filter (snd >> isSpace >> not)
            |> Array.rev
            |> List.ofArray

        disk
        |> reallocateBlockSmarter blocksInReverserOrder
        |> computeChecksum
        |> string
