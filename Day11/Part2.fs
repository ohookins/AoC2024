module Day11.Part2

open System.IO

let ITERATIONS = 75

let parseInput (filename: string) =
    let stones = Array.head (File.ReadAllLines filename)
    stones.Split(' ')
    |> Seq.fold (fun (acc: Map<int64,int64>) elem ->
        acc.Add(System.Int64.Parse(elem), 1L) // assume each number only occurs once in the input
    ) Map.empty

let nextStone (stone: int64) =
    match stone with
    | s when s = 0L -> seq { 1L }
    | s when s.ToString().Length % 2 = 0 ->
        let digits = s.ToString()
        let length = digits.Length
        let left = digits.Substring(0, length/2)
        let right = digits.Substring(length/2, length/2)

        seq { System.Int64.Parse(left); System.Int64.Parse(right)}
    | _ -> seq { stone * 2024L }


let blink (stones: Map<int64,int64>) =
    stones
    |> Map.fold (fun (state: Map<int64,int64>) stone count ->
        let nextStones = nextStone stone

        // ensure we add the new first stone value to any existing if we find one
        let firstStone = Seq.head nextStones
        let firstCount =
            match (state.TryFind firstStone) with
            | Some x -> x + count
            | None -> count

        match (Seq.length nextStones) with
        | 2 ->    
            // same for second stone, if there are two (i.e. it was split)
            let secondStone = Seq.item 1 nextStones

            // both stones are the same, add the count twice
            if firstStone = secondStone then
                state.Add(secondStone, (firstCount + count))
            else
                let secondCount =
                    match (state.TryFind secondStone) with
                    | Some x -> x + count
                    | None -> count
                state.Add(firstStone, firstCount).Add(secondStone, secondCount)
        | _ ->
            state.Add(firstStone, firstCount)
    ) Map.empty

let solve (filename: string): unit =
    let stones = parseInput filename
    let result =
        Seq.init ITERATIONS (fun e -> e)
        |> Seq.fold (fun acc elem -> blink acc) stones
        |> Map.fold (fun state k v -> state + v) 0L

    printfn "%d" result
