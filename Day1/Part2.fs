module Day1.Part2

open System
open System.IO

let solve (filename: string): unit =
    let lines = File.ReadAllLines filename
    let list = Seq.toList lines

    let split = Seq.map (fun (n: string) -> n.Split(" ", StringSplitOptions.RemoveEmptyEntries)) list
    let first = Seq.map (fun (a: array<string>) -> int(a[0])) split
    let second = Seq.map (fun (a: array<string>) -> int(a[1])) split

    let similarities = Seq.map (fun (a: int) -> a * Seq.length(Seq.where (fun (b: int) -> a = b) second)) first

    let answer = Seq.sum similarities

    printfn "%d" answer