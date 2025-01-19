module Day11.Part1

open System.IO
open System

let ITERATIONS = 25

let parseInput (filename: string) =
    File.ReadLines filename
    |> Seq.map (fun line ->
        line.Split(' ')
        |> Seq.map (fun elem ->
            System.Int64.Parse(elem)
        )
    )
    |> Seq.head

let blink (stones: seq<int64>) =
    stones
    |> Seq.map (fun stone ->
        match stone with
        | s when s = 0L -> seq { 1L }
        | s when s.ToString().Length % 2 = 0 ->
            let digits = s.ToString()
            let length = digits.Length
            let left = digits.Substring(0, length/2)
            let right = digits.Substring(length/2, length/2)

            seq { System.Int64.Parse(left); System.Int64.Parse(right)}
        | _ -> seq { stone * 2024L }
    )
    |> Seq.collect id

let solve (filename: string): unit =
    let stones = parseInput filename
    let result =
        Seq.init ITERATIONS (fun e -> 0L)
        |> Seq.fold (fun acc _ -> blink acc) stones
        |> Seq.length

    printfn "%d" result
