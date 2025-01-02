module Day9.Part1

open System.IO
open System

type File = {
    id: int64; // the checksum will likely get big so I'm lazy and using int64 here
    blocks: int64;
}

// This is now a hack, since I want to record the id and the block count with the Data block.
// We have to determine the file ID as we are parsing the file for the first time, as once we
// start moving blocks around we will lose their position in the file.
type Block =
    | Data of File
    | Free of File

let debug msg =
    let ts = DateTime.Now.ToString("HH:mm:ss.fff")
    // printfn "%s: %s" ts msg
    ()

let parseInput (filename: string) =
    File.ReadAllLines filename
    |> Array.head // there's just a single line of input
    |> Seq.chunkBySize 2 // one data block and one free block, always in pairs
    |> Seq.indexed
    |> Seq.map (fun (index, c) ->
        // last element can be a lone data element
        match c.Length with
        | 2 -> 
            seq { Data({id=index; blocks=System.Int64.Parse(c[0].ToString())}); Free({id=0; blocks=System.Int64.Parse(c[1].ToString())}) }
        | _ ->
            seq { Data({id=index; blocks=System.Int64.Parse(c[0].ToString())}) }
    )
    |> Seq.collect id // flatten
    |> Seq.toList

[<TailCall>]
let rec defrag (files: list<File>) (blocks: list<Block>) =
    debug $"number of files: {List.length files}, number of blocks: {List.length blocks}"
    let head = List.head blocks
    let tail = List.tail blocks

    // Too many nested matches.
    match List.length tail with
    | 0 ->
        match head with
        | Data d -> d :: files
        | Free f -> files

    | _ ->
        match head with
        | Data d ->
            let newFiles = d :: files
            debug "defrag newFiles tail"
            defrag newFiles tail

        | Free f ->
            let freeBlocks = f.blocks
            debug $"free blocks {freeBlocks}"
            let lastElem = List.last tail
            let newBlocks = List.removeAt ((List.length tail) - 1) tail

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
                let newFiles = {id=d.id; blocks=writtenBlocks } :: files 
                debug $"defragged {writtenBlocks} blocks"

                match remainingBlocks with
                | 0L -> 
                    debug "defrag newFiles newBlocks"
                    defrag newFiles newBlocks
                | _ ->
                    // some left over blocks from the current file
                    let revNewBlocks = List.rev newBlocks
                    let revAppendedNewBlocks = Data({id=d.id; blocks=remainingBlocks}) :: revNewBlocks
                    let appendedNewBlocks = List.rev revAppendedNewBlocks
                    debug $"defrag newFiles appendedNewBlocks ({appendedNewBlocks |> List.length})"
                    defrag newFiles appendedNewBlocks

let rec makeChecksum (startBlock: int64) (files: List<File>) =
    let head = List.head files
    let tail = List.tail files

    // figure out how much we need to multiply the ID by, given the start block
    // and end block numbers for the file. It's the old "sum numbers 1...100" thing.
    let blockCount = head.blocks
    let endBlock = startBlock + blockCount
    let blockMultiplier = (startBlock + endBlock) * blockCount / 2L
    let blockChecksum = blockMultiplier * head.id

    match List.length tail with
    | 0 -> blockChecksum
    | _ -> blockChecksum + makeChecksum (endBlock+1L) tail

let solve (filename: string): unit =
    let checksum =
        parseInput filename
        |> defrag List.empty
        |> List.rev
        |> List.map (fun f ->
            for i = 0 to (int(f.blocks) - 1) do
                printf "%d" f.id
        ) |> ignore
        // |> makeChecksum 0L

    printfn ""
