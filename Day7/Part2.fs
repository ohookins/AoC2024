module Day7.Part2

open System.IO

let parseExpressions lines =
    lines
    |> Seq.map (fun (line: string) ->
        let split = line.Split(@": ")
        let lhs = split[0]
        let rhs =
            split[1].Split(@" ")
            |> Array.toSeq

        Seq.append (seq { lhs }) rhs
    )
    |> Seq.map (fun s ->
        // convert from regex group to string, trim whitespace, convert to ints
        s |> Seq.map (fun i -> int64(i.Trim()))
    )

let concatInts (a:int64) (b:int64): int64 =
    int64(a.ToString() + b.ToString())

// I can't think of a better way to solve part 2, so it's going to be recursive brute force
// considering all potential paths.
let rec solveNextTerm (terms: seq<int64>): seq<int64> =
    let first = Seq.head terms
    let remainingTerms = Seq.tail terms 

    match Seq.length remainingTerms with
    | 1 ->
        // One remaining term means only considering the three types of operators in a sequence.
        let finalTerm = Seq.head remainingTerms
        seq { first + finalTerm; first * finalTerm; concatInts first finalTerm }
    | _ ->
        let nextTerm = Seq.head remainingTerms
        let newRemainingTerms = Seq.tail remainingTerms

        // We calculate the three different operator types for the current and next term,
        // then for each of these possibilities, recursively call this function for each.
        // This results in nested sequences so we need to flatten them with collect before
        // returning a potentially large but flat sequence of all possible solutions.
        seq { first + nextTerm; first * nextTerm; concatInts first nextTerm }
        |> Seq.map (fun s ->
            let terms = Seq.append (seq { s }) newRemainingTerms
            solveNextTerm terms
        )
        |> Seq.collect (fun s -> s)
        

let expressionIsCorrect (exp: seq<int64>) =
    let lhs = Seq.head exp
    let terms = Seq.tail exp

    let correctSolutions = 
        solveNextTerm terms
        |> Seq.filter (fun s -> s = lhs)
        |> Seq.length

    match correctSolutions with
    | 0 -> None
    | _ -> Some(lhs)

let solve (filename: string): unit =
    let mutable count = 0

    let lines = File.ReadAllLines filename
    let correctExpressions =
        parseExpressions lines
        |> Seq.map (fun e ->
            printf "\rSolutions calculated: %d" count
            count <- count + 1
            expressionIsCorrect e
        )
        |> Seq.choose id
        |> Seq.sum

    printfn "\n%d" correctExpressions