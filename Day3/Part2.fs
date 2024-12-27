module Day3.Part2

open System
open System.IO
open System.Text.RegularExpressions

let rec removeSkippedSequences (s: seq<string>, skip: bool): seq<string> =
    match Seq.isEmpty s with
    | true -> Seq.empty
    | false -> 
        let head = Seq.head s
        let tail =
            match head with
            | "don't()" -> removeSkippedSequences(Seq.tail s, true)
            | "do()" -> removeSkippedSequences(Seq.tail s, false)
            | _ ->
                match skip with
                | true -> (removeSkippedSequences(Seq.tail s, true))
                | false -> Seq.append (seq { head }) (removeSkippedSequences(Seq.tail s, false))

        tail

let solve (filename: string): unit =
    let lines = File.ReadAllLines filename
    // join it into one long string, in case operations are split over lines
    let text = String.Join("", lines)

    let mainPattern = Regex "mul\(\d+,\d+\)|do\(\)|don't\(\)"
    let mulPattern = Regex "mul\((\d+),(\d+)\)"

    let matches = mainPattern.Matches text
    let parsedOps =
        matches
        |> Seq.cast<Match>
        |> Seq.map (fun m -> m.Value)

    let result =
        removeSkippedSequences(parsedOps, false)
        |> Seq.map (fun op ->
            let m = mulPattern.Match op
            int(m.Groups[1].ToString()) * int(m.Groups[2].ToString())
        )
        |> Seq.sum

    printfn "%d" result