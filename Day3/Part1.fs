module Day3.Part1

open System
open System.IO
open System.Text.RegularExpressions

let solve (filename: string): unit =
    let lines = File.ReadAllLines filename
    // join it into one long string, in case operations are split over lines
    let text = String.Join("", lines)

    let pattern = Regex "mul\((\d+),(\d+)\)"

    let matches = pattern.Matches text
    let result =
        matches
        |> Seq.cast<Match>
        |> Seq.map (fun m -> int(m.Groups[1].ToString()) * int(m.Groups[2].ToString()))
        |> Seq.sum

    printfn "%d" result