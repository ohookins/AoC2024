module Day10.Part1

open System.IO
open System

let parseInput (filename: string) =
    let lines = File.ReadAllLines filename
    Array2D.init lines[0].Length lines.Length (
        fun x y -> System.Int32.Parse (string(lines[y][x]))
    )

let findTrailHeads (grid: int[,]) =
    seq {
        for x in 0 .. Array2D.length1 grid - 1 do
            for y in 0 .. Array2D.length2 grid - 1 do
                if grid.[x, y] = 0 then
                    yield (x, y)
    }

let getPossibleMoves (grid: int[,]) (x: int) (y: int) =
    // For some reason, using a sequence here and lazy evaluation leads to
    // only some moves being checked. Lists are eagerly evaluated. 
    [(x-1, y); (x+1, y); (x, y-1); (x, y+1)]
    |> List.filter (fun (moveX, moveY) ->
        moveX >= 0 && moveX < Array2D.length1 grid && moveY >= 0 && moveY < Array2D.length2 grid
    )

let rec findTrails (grid: int[,]) (x: int) (y: int) =
    let current = grid[x, y]
    // printfn "findTrails x:%d y:%d current:%d next:%d"x y current (current + 1)

    getPossibleMoves grid x y
    |> List.map (fun (moveX, moveY) ->
        let move = grid.[moveX, moveY]
        // printfn "considering move to %d %d with step %d -> %d" moveX moveY current move

        match move with // this is the next Step along the path
        | next when next = 9 && next = current+1 ->
            // printfn "found end of trail (%d -> %d)" current next
            Set.empty.Add (moveX,moveY)
        | next when next = current + 1 ->
            // printfn "found next step (%d -> %d)" current next
            findTrails grid moveX moveY
        | _ -> Set.empty
    )
    |> Set.unionMany

let solve (filename: string): unit =
    let grid = parseInput filename

    let trailCount =
        findTrailHeads grid
        // |> Seq.take 1
        |> Seq.map (fun (x, y) ->
            findTrails grid x y
            |> Set.count
        )
        |> Seq.sum

    printfn "%d" trailCount
