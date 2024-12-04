namespace AoC2024

open Utils

module Day04 =

    let parseInput (input: string) : char array array = input |> lines |> Array.map Seq.toArray

    let rec countOccurrences (acc: int) (line: char list) : int =
        match line with
        | []
        | _ :: []
        | _ :: _ :: []
        | _ :: _ :: _ :: [] -> acc
        | 'X' :: 'M' :: 'A' :: 'S' :: cs
        | 'S' :: 'A' :: 'M' :: 'X' :: cs -> countOccurrences (acc + 1) (List.tail line)
        | _ -> countOccurrences (acc) (List.tail line)

    let diagonalize (board: char array array) : char array array =
        board
        |> Array.mapi (fun i arr ->
            Array.concat [ [| Array.replicate (board.Length - i) '.' |]
                           [| arr |]
                           [| Array.replicate i '.' |] ]
            |> Array.concat)

    let count (board: char array array) : int =
        board
        |> Array.map (List.ofArray >> countOccurrences 0)
        |> Array.sum

    let part1 (input: string) : string =
        let hej = input |> parseInput

        let hejT = Array.transpose hej

        let hejD = Array.transpose (diagonalize hej)
        let hejD' = Array.transpose (diagonalize (Array.rev hej))

        ((count hej)
         + (count hejT)
         + (count hejD)
         + (count hejD'))
        |> string

    let threeByThreeIze (board: char array array) (pos as (x, y): Position) : char array array option =
        if x <= 0 || x >= (board.Length - 1) then
            None
        elif y <= 0 || y >= (board[0].Length - 1) then
            None
        else
            let kernel =
                board[x - 1 .. x + 1]
                |> Array.map (fun a -> a[y - 1 .. y + 1])

            Some [| [| kernel[0][0]
                       kernel[1][1]
                       kernel[2][2] |]
                    [| kernel[2][0]
                       kernel[1][1]
                       kernel[0][2] |] |]

    let findAPositions (board: char array array) : Position array =
        board
        |> Array.mapi (fun x board' ->
            board'
            |> Array.mapi (fun y c -> if c = 'A' then Some(x, y) else None))
        |> Array.concat
        |> Array.choose id

    let isMas (line: char list) : bool =
        match line with
        | 'M' :: 'A' :: 'S' :: []
        | 'S' :: 'A' :: 'M' :: [] -> true
        | _ -> false

    let countMas (board: char array array) : int =
        board
        |> Array.map (List.ofArray >> isMas)
        |> Array.forall id
        |> (fun a -> if a then 1 else 0)

    let part2 (input: string) : string =
        let board = input |> parseInput

        let positions = board |> findAPositions

        positions
        |> Array.choose (threeByThreeIze board)
        |> Array.map countMas
        |> Array.sum
        |> string
