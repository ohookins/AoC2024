module Day5.Graph

type Rule =
    struct
        val first: int
        val second: int
        new(first: int, second: int) = { first = first; second = second}
    end

type Graph = Map<int, Set<int>>

let addEdge graph node1 node2 =
    // Update the origin node's neighbor set (or initialize if it doesn't exist)
    let updateNode node neighbour graph =
        graph
        |> Map.change node (function
            | Some neighbors -> Some (Set.add neighbour neighbors)
            | None -> Some (Set.singleton neighbour))
    
    graph
    |> updateNode node1 node2

// findEnds takes the sequence of rules, and using set logic finds the update number
// that only appears on the left-hand side of any of the rules (i.e. this can only be
// the starting node in the graph) and the update number that only appears on the right-hand
// side of any of the rules (i.e. this can only be the ending node in the graph).
let findEnds (rules: seq<Rule>) =
    let left =
        rules
        |> Seq.fold (fun acc (r: Rule) -> Set.add r.first acc) Set.empty

    let right =
        rules
        |> Seq.fold (fun acc (r: Rule) -> Set.add r.second acc) Set.empty

    // this is super lazy, but I'll assume there are no errors in the notation
    // and just use MinimumElement to get the only element in the set
    ((left - right).MinimumElement, (right - left).MinimumElement)
     
let rec navigate (currentNode: int) (graph: Graph): seq<seq<int>> =
    let currentPath = seq { currentNode }

    match graph.TryFind(currentNode) with
    | Some children -> 
        children
        |> Set.toSeq
        |> Seq.collect (fun child ->
            navigate child graph
            |> Seq.map (fun path ->
                Seq.append currentPath path
            )
        )
    | None -> seq { currentPath }