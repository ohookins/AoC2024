module Day4.Part2

open System.IO

let isWithinBounds(x, y, grid: char array2d) =
    x > 0 && x < grid.GetLength(0)-1 && y > 0 && y < grid.GetLength(1)-1

let findMasAt (x, y, grid: char array2d) =
    let s = Set(seq { 'M'; 'S' })

    if isWithinBounds(x, y, grid) then
        let oneCross = Set(seq { grid.[x-1,y-1]; grid.[x+1,y+1] })
        let twoCross = Set(seq { grid.[x-1,y+1]; grid.[x+1,y-1] })
        oneCross.Equals(s) && twoCross.Equals(s)
    else
        false

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
            | 'A' -> findMasAt(x,y,grid)
            | _ -> false
        )

    let result =
        Seq.init (counts.GetLength(1)) (fun y ->
            Seq.init (counts.GetLength(0)) (fun x -> counts.[x,y])
            |> Seq.filter id
            |> Seq.length
        )
        |> Seq.sum

    printfn "%d" result