module Day9.Part1

open System.IO
open System

type File = {
    id: int;
    blocks: int;
}

// This is now a hack, since I want to record the id and the block count with the Data block.
// We have to determine the file ID as we are parsing the file for the first time, as once we
// start moving blocks around we will lose their position in the file.
type Block =
    | Data of File
    | Free of File

let debug msg =
    let ts = DateTime.Now.ToString("HH:mm:ss.fff")
    printfn "%s: %s" ts msg

let parseInput (filename: string) =
    File.ReadAllLines filename
    |> Array.head // there's just a single line of input
    |> Seq.chunkBySize 2 // one data block and one free block, always in pairs
    |> Seq.indexed
    |> Seq.map (fun (index, c) ->
        // last element can be a lone data element
        match c.Length with
        | 2 -> 
            seq { Data({id=index; blocks=System.Int32.Parse(c[0].ToString())}); Free({id=0; blocks=System.Int32.Parse(c[1].ToString())}) }
        | _ ->
            seq { Data({id=index; blocks=System.Int32.Parse(c[0].ToString())}) }
    )
    |> Seq.collect id // flatten

[<TailCall>]
let rec defrag (files: seq<File>) (blocks: seq<Block>) =
    debug $"number of files: {Seq.length files}, number of blocks: {Seq.length blocks}"
    let head = Seq.head blocks
    let tail = Seq.tail blocks

    // Too many nested matches.
    match Seq.length tail with
    | 0 ->
        match head with
        | Data d -> Seq.append files (seq { d })
        | Free f -> files

    | _ ->
        match head with
        | Data d ->
            let newFiles = Seq.append files (seq { d })
            debug "defrag newFiles tail"
            defrag newFiles tail

        | Free f ->
            let freeBlocks = f.blocks
            debug $"free blocks {freeBlocks}"
            let lastElem = Seq.last tail
            let newBlocks = Seq.removeAt ((Seq.length tail) - 1) tail

            match lastElem with
            | Free _ ->
                // Last element in the block sequence is free space, which we don't care about.
                // Do the following:
                // 1. strip the free block from the end of the original sequence
                // 2. recurse again
                debug $"recursing into defrag files newBlocks"
                defrag files newBlocks
            | Data d ->
                // Last element is a file. We need to put as many blocks of it as will fit in
                // the current space, and then put back any left over back onto the block sequence
                // if we run out of space.
                let writtenBlocks = min d.blocks freeBlocks
                let remainingBlocks = d.blocks - writtenBlocks
                let newFiles = Seq.append files (seq {{id=d.id; blocks=writtenBlocks }}) 
                debug $"defragged {writtenBlocks} blocks"

                match remainingBlocks with
                | 0 -> 
                    debug "defrag newFiles newBlocks"
                    defrag newFiles newBlocks
                | _ ->
                    // some left over blocks from the current file
                    let appendedNewBlocks = Seq.append newBlocks (seq { Data({id=d.id; blocks=remainingBlocks}) })
                    debug $"defrag newFiles appendedNewBlocks ({appendedNewBlocks |> Seq.length})"
                    defrag newFiles appendedNewBlocks

let solve (filename: string): unit =
    let blocks =
        parseInput filename
        |> defrag Seq.empty

    printfn "%d" (Seq.length blocks)
