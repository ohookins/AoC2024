module Day5.Part1

open System.IO
open System.Text.RegularExpressions

type UpdateStatus =
    | Correct
    | Incorrect

type Ordering =
    struct
        val first: int
        val second: int
        new(first: int, second: int) = { first = first; second = second}
    end

let orderingRegex = Regex("(\\d+)\\|(\\d+)")
let updateRegex = Regex("(\\d+,)+\\d+")

let parseRules lines =
    lines
    |> Seq.map (fun line ->
        match orderingRegex.Match(line) with
        | m when m.Success -> Some(Ordering(int(m.Groups[1].ToString()), int(m.Groups[2].ToString())))
        | _ -> None
    )
    |> Seq.filter (fun s -> s.IsSome)
    |> Seq.map (fun s -> s.Value)

let parseUpdates lines =
    lines
    |> Seq.map (fun line ->
        match updateRegex.Match(line) with
        | m when m.Success -> Some(seq(line.Split(",")))
        | _ -> None
    )
    |> Seq.filter (fun s -> s.IsSome)
    |> Seq.map (fun s -> 
        s.Value
        |> Seq.map (fun v -> int(v))
    )


let seenUpdateThatShouldComeLater(currentUpdate, seenUpdates: Set<int>, rules: seq<Ordering>) =
    rules
    |> Seq.filter (fun r -> r.first = currentUpdate)
    |> Seq.map(fun r -> seenUpdates.Contains(r.second))
    |> Seq.exists id

let rec updateIsCorrect(updateSeq, seenUpdates: Set<int>, rules): UpdateStatus =
    match Seq.isEmpty updateSeq with
    | true -> Correct
    | false ->
        let currentUpdate = Seq.head updateSeq
        match seenUpdateThatShouldComeLater(currentUpdate, seenUpdates, rules) with
        | false -> updateIsCorrect(Seq.tail updateSeq, seenUpdates.Add(currentUpdate), rules)
        | true -> Incorrect

let solve (filename: string): unit =
    let lines = File.ReadAllLines filename
    
    let rules = parseRules lines
    let updates = parseUpdates lines

    let correctUpdates =
        updates
        |> Seq.filter (fun u ->
            updateIsCorrect(u, Set.empty, rules) = Correct
        )
        |> Seq.map (fun u ->
            let len = Seq.length u
            let middle = len/2
            Seq.item(middle) u
        )
        |> Seq.sum
        

    printfn "%d" correctUpdates