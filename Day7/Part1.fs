module Day7.Part1

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

// Recursively solve "reversal" of the expression one term at a time, by
// either dividing or substracting the current term depending on what works.
// The final term must equal the LHS of the expression or it is invalid.
let rec solveOneTerm (lhs: int64) remainingTerms =
    let currentTerm = Seq.head remainingTerms
    let newRemainingTerms = Seq.tail remainingTerms 

    match Seq.length remainingTerms with
    | 1 ->
        //printfn "%A" remainingTerms
        //printfn "LHS: %d, current: %d" lhs currentTerm
        lhs = currentTerm
    | _ ->
        // check for divisibility
        match (lhs % currentTerm) = 0 with
        | true ->
            // If there is perfect division possible, that may not actually lead to the answer, so in this
            // case let's consider both branches all the way to the end and decide which one to use.
            let divLhs = lhs / currentTerm
            let minusLhs = lhs - currentTerm
            let divSolveOneTerm = solveOneTerm divLhs newRemainingTerms
            let minusSolveOneTerm = solveOneTerm minusLhs newRemainingTerms

            divSolveOneTerm || minusSolveOneTerm
        | false ->
            // if we end up negative does it matter? could short-cut some calculations but it may not be worth it.
            let newLhs = lhs - currentTerm
            solveOneTerm newLhs newRemainingTerms

let expressionIsCorrect (exp: seq<int64>) =
    let lhs = Seq.head exp
    
    // process terms from the end
    let terms =
        Seq.tail exp
        |> Seq.rev
    
    match solveOneTerm lhs terms with
    | true -> Some(lhs)
    | false -> None

let solve (test: bool): unit =
    let filename =
        match test with
        | true -> @"Day7/test.txt"
        | false -> @"Day7/input.txt"

    let lines = File.ReadAllLines filename
    let correctExpressions =
        parseExpressions lines
        |> Seq.map (fun e ->
            //printfn "%A: %A" e (expressionIsCorrect e)
            expressionIsCorrect e
        )
        |> Seq.choose id
        |> Seq.sum

    printfn "%d" correctExpressions