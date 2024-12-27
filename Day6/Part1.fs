module Day6.Part1

open System.IO

type Direction =
    | North
    | South
    | East
    | West

type NextToken =
    | Obstacle
    | Open
    | OffGrid

let getNextPosition pos dir =
    let x =
        match dir with
        | East -> (fst pos) + 1
        | West -> (fst pos) - 1
        | _ -> (fst pos)
    let y =
        match dir with
        | North -> (snd pos) - 1
        | South -> (snd pos) + 1
        | _ -> (snd pos)
    (x,y)

let getNextToken pos grid =
    let gridXmax = Array2D.length1 grid - 1
    let gridYmax = Array2D.length2 grid - 1
    let isOffGrid = (fst pos) < 0 || (snd pos) < 0 || (fst pos) > gridXmax || (snd pos) > gridYmax

    if isOffGrid then
        OffGrid
    else
        match Array2D.get grid (fst pos) (snd pos) with
        | '#' -> Obstacle
        | _ -> Open

let getNextDirection dir token =
    match token with
    | Obstacle ->
        match dir with
        | North -> East
        | East -> South
        | South -> West
        | West -> North
    | _ -> dir

let rec navigate pos dir visited grid =
    let newVisited = Set.add pos visited
    let nextPosition = getNextPosition pos dir
    let nextToken = getNextToken nextPosition grid
    let nextDirection = getNextDirection dir nextToken
    //printfn "newVisited: %A, nextPosition: %A, nextToken: %A, nextDirection: %A" newVisited nextPosition nextToken nextDirection

    match nextToken with
    | OffGrid -> newVisited
    | Open -> navigate nextPosition nextDirection newVisited grid
    | Obstacle -> navigate pos nextDirection newVisited grid

// This is a fairly garbage way of finding the ^ character in the grid.
let startingPosition grid =
    let i = Array2D.length1 grid
    let j = Array2D.length2 grid

    seq { 0..i-1 }
    |> Seq.map (fun i ->
        seq { 0..j-1 }
        |> Seq.map (fun j ->
            match Array2D.get grid i j with
            | '^' -> Some((i,j))
            | _ -> None
        )
        |> Seq.choose id
        |> Seq.tryHead
    )
    |> Seq.choose id
    |> Seq.head

let solve (filename: string): unit =
    let lines = File.ReadAllLines filename
    let grid = Array2D.init lines[0].Length lines.Length (fun x y -> lines[y][x])

    let start = startingPosition grid

    //printfn "%A" start

    let visited = navigate start North Set.empty grid

    //printfn "%A" visited

    printfn "%d" (Set.count visited)