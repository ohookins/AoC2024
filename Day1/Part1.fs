module Day1.Part1

open System
open System.IO

let solve (test: bool): unit =
    let filename =
        match test with
        | true -> @"Day1/test.txt"
        | false -> @"Day1/input.txt"

    let lines = File.ReadAllLines filename
    let list = Seq.toList lines

    let split = Seq.map (fun (n: string) -> n.Split(" ", StringSplitOptions.RemoveEmptyEntries)) list
    let first = Seq.map (fun (a: array<string>) -> int(a[0])) split
    let firstSorted = Seq.sort first

    let second = Seq.map (fun (a: array<string>) -> int(a[1])) split
    let secondSorted = Seq.sort second

    let zipSeq = Seq.zip firstSorted secondSorted
    let diffSeq = Seq.map (fun (elem: (int * int)) -> Math.Abs(fst(elem) - snd(elem))) zipSeq

    let answer = Seq.reduce (fun acc elem -> acc + elem) diffSeq

    printfn "%d" answer