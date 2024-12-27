module Day6.Part2

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

type LoopingState =
    | Looping
    | NotLooping

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

// initialNavigate navigates the grid with the guard, but without consideration for additional
// obstacles in the path - therefore it only records the set of visited positions and does not
// include any directional information with the visited positions.
let rec initialNavigate pos dir visited grid =
    let newVisited = Set.add pos visited
    let nextPosition = getNextPosition pos dir
    let nextToken = getNextToken nextPosition grid
    let nextDirection = getNextDirection dir nextToken

    match nextToken with
    | OffGrid -> newVisited
    | Open -> initialNavigate nextPosition nextDirection newVisited grid
    | Obstacle -> initialNavigate pos nextDirection newVisited grid

let rec obstacleNavigate pos dir visited grid =
    // Key difference with initialNavigate is that we maintain the direction in the set of visited
    // locations. This is how we detect looping - it's not enough to be in the same location - we need
    // to be in the same location and travelling in the same direction.
    let (x, y) = pos
    let currentPosDir = (x, y, dir)
    let loopingState = 
        match Set.contains currentPosDir visited with
        | true -> Looping
        | false -> NotLooping

    // calculate the other movement variables
    let newVisited = Set.add currentPosDir visited
    let nextPosition = getNextPosition pos dir
    let nextToken = getNextToken nextPosition grid
    let nextDirection = getNextDirection dir nextToken

    match loopingState with
    | Looping -> Looping
    | NotLooping ->
        match nextToken with
        | OffGrid -> NotLooping
        | Open -> obstacleNavigate nextPosition nextDirection newVisited grid
        | Obstacle -> obstacleNavigate pos nextDirection newVisited grid

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
    let obstacles =
        initialNavigate start North Set.empty grid
        |> Set.remove start // can't put an obstacle where the guard starts
        |> Set.toSeq
        |> Seq.map (fun (x, y) ->
            // Add current potential obstacle to a new copy of the grid
            let newGrid = grid |> Array2D.copy
            Array2D.set newGrid x y '#'
            obstacleNavigate start North Set.empty newGrid
        )
        |> Seq.filter (fun s -> s = Looping)
        |> Seq.length


    printfn "%d" obstacles