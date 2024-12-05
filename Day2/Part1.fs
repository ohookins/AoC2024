module Day2.Part1

open System
open System.IO

let allAscending (s: seq<int>): bool =
    s |> Seq.pairwise |> Seq.forall (fun (a, b) -> b > a)

let allDescending (s: seq<int>): bool =
    s |> Seq.pairwise |> Seq.forall (fun (a, b) -> a > b)

let sufficientDifference (s: seq<int>): bool =
    s |> Seq.pairwise |> Seq.forall (fun (a, b) -> abs(a-b) > 0 && abs(a-b) <= 3)

let solve (test: bool): unit =
    let filename =
        match test with
        | true -> @"Day2/test.txt"
        | false -> @"Day2/input.txt"

    let lines = File.ReadAllLines filename
    let list = Seq.toList lines

    let split = 
        list
        |> Seq.map (fun (n: string) ->
            n.Split(" ", StringSplitOptions.RemoveEmptyEntries)
            |> Seq.map int)

    let satisfiesAllRules =
        split 
        |> Seq.map (fun (a: seq<int>) -> (allAscending(a) || allDescending(a)) && sufficientDifference(a))
        |> Seq.where id
        |> Seq.length

    printfn "%d" satisfiesAllRules