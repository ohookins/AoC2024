module Day4.Part1

open System
open System.IO

type Direction =
    | North
    | NorthEast
    | East
    | SouthEast
    | South
    | SouthWest
    | West
    | NorthWest

let isWithinBounds(x, y, grid: char array2d) =
    x >= 0 && x < grid.GetLength(0) && y >= 0 && y < grid.GetLength(1)

let move (x, y, direction: Direction, grid: char array2d) =
    let vel: Tuple<int,int> =
        match direction with
        | North -> (0,-1)
        | NorthEast -> (1,-1)
        | East -> (1,0)
        | SouthEast -> (1,1)
        | South -> (0,1)
        | SouthWest -> (-1,1)
        | West -> (-1,0)
        | NorthWest -> (-1,-1)
 
    let xEnd = x + fst(vel)*3
    let yEnd = y + snd(vel)*3

    if isWithinBounds(xEnd, yEnd, grid) then
        grid.[x+fst(vel),y+snd(vel)] = 'M' && grid.[x+fst(vel)*2,y+snd(vel)*2] = 'A' && grid.[x+fst(vel)*3,y+snd(vel)*3] = 'S'
    else
        false

let findXmasAt (x, y, grid: char array2d) =
    seq { North; NorthEast; East; SouthEast; South; SouthWest; West; NorthWest }
    |> Seq.map (fun direction -> move(x, y, direction, grid))
    |> Seq.filter id
    |> Seq.length

let solve (test: bool): unit =
    let filename =
        match test with
        | true -> @"Day4/test.txt"
        | false -> @"Day4/input.txt"

    let lines = File.ReadAllLines filename
    let grid = Array2D.init lines[0].Length lines.Length (fun x y -> lines[y][x])

    let counts =
        grid
        |> Array2D.mapi (fun x y v ->
            match v with
            | 'X' -> findXmasAt(x,y,grid)
            | _ -> 0
        )

    let result =
        Seq.init (counts.GetLength(1)) (fun y ->
            Seq.init (counts.GetLength(0)) (fun x -> counts.[x,y])
            |> Seq.sum
        )
        |> Seq.sum

    printfn "%d" result