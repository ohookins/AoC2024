module Day11.Part1

open System.IO
open System

let parseInput (filename: string) =
    File.Read
    // File.ReadAllLines filename

    // System.Int64.Parse

let solve (filename: string): unit =
    let stones = parseInput filename

    printfn "%A" stones
