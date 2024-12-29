module Day8.Part2

open System.IO

// (minX, minY, maxX, maxY)
let findBounds (filename: string) =
    let f = File.ReadAllLines filename
    (0, 0, String.length (Array.head f) - 1, f.Length - 1)

// Read through all lines, with each line representing the y axis and each character
// representing the x axis. We assemble a map of character to set<int * int>. The sets
// each represent the locations a given character in the map has been found at. The map
// is indexed by the character being used for that antenna.
// This lets us generate the combinatorial product of each antenna letter within each set
// later on, and determine the antinode positions for each possible pair of antennae.
let parseInputs (filename: string) =
    File.ReadAllLines filename
    |> Array.toSeq
    |> Seq.indexed
    |> Seq.fold (fun (acc1: Map<char, Set<int * int>>) (y, line) ->
        line.ToCharArray()
        |> Array.toSeq
        |> Seq.indexed
        |> Seq.fold (fun (acc2: Map<char, Set<int * int>>) (x, c: char) ->
            match c with
            | '.' -> acc2 // ignore dot characters
            | _ ->
                match acc2.TryFind c with
                | Some s1 -> acc2.Add (c, Set.add (x, y) s1) 
                | None ->
                    let s = Set.ofList [ (x, y) ]
                    acc2.Add (c, s)
        ) acc1
    ) Map.empty

// permutations generates all permutations of the members of the given sequence.
// It's limited to the type we are working with in this solution - tuple of two ints.
let rec permutations (source: seq<int * int>) =
    let (x1,y1) = Seq.head source
    let tail = Seq.tail source

    match Seq.length tail with
    // If we only have one remaining element in the sequence, we don't recurse into the
    // function for that - there's nothing left to combine it with except the previous
    // element.
    | 1 ->
        let (x2,y2) = Seq.head tail
        seq { (x1,y1,x2,y2) }
    // Two or more elements - we have two things to do.
    // 1. Multiple the current element with the remaining elements.
    // 2. Take the tail and call this function recursively.
    | _ ->
        let next = permutations tail
        let this =
            tail
            |> Seq.map (fun (x2,y2) -> (x1,y1,x2,y2))
        Seq.append this next

let within_bounds (point:int*int) (bounds:int*int*int*int) =
    let (x,y) = point
    let (minX, minY, maxX, maxY) = bounds
    x >= minX && x <= maxX && y >= minY && y <= maxY

// This has to be different to part 1, as we are calculating the position of every antinode along the
// path until we are outside the bounds. We still need the vector between v1 and v2, in both directions.
// The simplest way I can think to do this, is to define an infinite sequence of each "resonance" point (i.e.
// the original antenna location plus the difference vector in that direction N number of times) and then
// use takeWhile to limit the resulting sequence to only being within the overall bounds of the map.
let calculate_antinodes (bounds: int*int*int*int) (s: seq<int*int*int*int>) =
    s
    |> Seq.map (fun (x1,y1,x2,y2) ->
        let v1_to_v2 = (x2 - x1, y2 - y1)
        let v2_to_v1 = (x1 - x2, y1 - y2)
        
        let a1 =
            Seq.initInfinite (fun index ->
                (x1 + (fst v1_to_v2) * (index + 1), y1 + (snd v1_to_v2) * (index + 1))
            )
            |> Seq.takeWhile (fun point -> within_bounds point bounds)
        let a2 =
            Seq.initInfinite (fun index ->
                (x2 + (fst v2_to_v1) * (index + 1), y2 + (snd v2_to_v1) * (index + 1))
            )
            |> Seq.takeWhile (fun point -> within_bounds point bounds)

        Seq.append a1 a2
    )
    |> Seq.fold (fun acc s ->
        Seq.append acc s
    ) Seq.empty

let solve (filename: string): unit =
    let inputs = parseInputs filename
    let bounds = findBounds filename

    // calculate all antinode positions for each character identifying the same antenna type
    let antinodes =
        inputs
        |> Map.toSeq
        |> Seq.map (fun (_,s) -> // antenna identifier is actually irrelevant at this point
            s
            |> Set.toSeq
            |> permutations
            |> calculate_antinodes bounds
        )
        |> Seq.fold (fun acc s -> Seq.append acc s) Seq.empty
        |> Set.ofSeq // remove duplicates

    printfn "%d" antinodes.Count