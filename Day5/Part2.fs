module Day5.Part2

open System.IO
open System.Text.RegularExpressions
open Day5.Graph

type UpdateStatus =
    | Correct
    | Incorrect

let ruleRegex = Regex("(\\d+)\\|(\\d+)")
let updateRegex = Regex("(\\d+,)+\\d+")

let parseRules lines =
    lines
    |> Seq.map (fun line ->
        match ruleRegex.Match(line) with
        | m when m.Success -> Some(Rule(int(m.Groups[1].ToString()), int(m.Groups[2].ToString())))
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

let seenUpdateThatShouldComeLater(currentUpdate, seenUpdates: Set<int>, rules: seq<Rule>) =
    rules
    |> Seq.filter (fun r -> r.first = currentUpdate)
    |> Seq.map(fun r -> seenUpdates.Contains(r.second))
    |> Seq.exists id

// The update is correct if, while processing each number, we look at all of the rules
// mentioning that number and the number that comes after it in the rule HASN'T been
// seen before. It's sort of a backwards check by elimination.
// It works recursively on the tail of the sequence of updates as I can't figure out how
// to otherwise process the rest of the sequence without mutating something.
let rec updateIsCorrect(updateSeq, seenUpdates: Set<int>, rules): UpdateStatus =
    match Seq.isEmpty updateSeq with
    | true -> Correct
    | false ->
        let currentUpdate = Seq.head updateSeq
        match seenUpdateThatShouldComeLater(currentUpdate, seenUpdates, rules) with
        | false -> updateIsCorrect(Seq.tail updateSeq, seenUpdates.Add(currentUpdate), rules)
        | true -> Incorrect

// Go through all of the rules and pick out all of them where BOTH numbers in
// the rule appear in the update sequence.
// I.e. this will result in sequence of all rules applicable to this particular update.
let filterRelevantRules(updateSeq, rules: seq<Rule>): seq<Rule> =
    let updateSet = Set(updateSeq)
    rules
    |> Seq.map (fun r ->
        if updateSet.Contains(r.first) && updateSet.Contains(r.second) then
            Some(r)
        else None
    )
    |> Seq.choose id


let correctUpdate(updateSeq, rules: seq<Rule>) =
    let relevantRules = filterRelevantRules(updateSeq, rules)

    // assemble the directed graph of rules applicable to this update
    let g =
        relevantRules
        |> Seq.fold (fun graph rule ->
            addEdge graph rule.first rule.second
        ) Map.empty
    
    // TODO: endNode not used, probably don't need it.
    let (startNode, endNode) = findEnds relevantRules

    // Navigate over the graph enumerating all possible paths, then
    // fold over all paths, pulling out the longest path and returning
    // that to the caller. The longest path should be the one that starts
    // at the start node, ends at the end node and covers all nodes in
    // the graph.
    navigate startNode g
    |> Seq.fold (fun acc s -> 
        if Seq.length s > Seq.length acc then
            s
        else
            acc
    ) Seq.empty

let solve (filename: string): unit =
    let lines = File.ReadAllLines filename
    
    let rules = parseRules lines
    let updates = parseUpdates lines

    let correctUpdates =
        updates
        |> Seq.filter (fun u ->
            updateIsCorrect(u, Set.empty, rules) = Incorrect
        )
        |> Seq.map (fun u ->
            correctUpdate(u, rules)
        )
        |> Seq.map (fun u ->
            u |> Seq.item (Seq.length u / 2)
        )
        |> Seq.sum
        
    printfn "%d" correctUpdates