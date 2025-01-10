module Day9.Part2

open System.IO
open System

type File = {
    id: int;
    size: int;
    address: int;
}

// This is now a hack, since I want to record the id and the block count with the Data block.
// We have to determine the file ID as we are parsing the file for the first time, as once we
// start moving blocks around we will lose their position in the file.
type Block =
    | Data of File
    | Free of File

// couldn't be bothered making a flag for this
let debugEnabled = false

let debug msg =
    let ts = DateTime.Now.ToString("HH:mm:ss.fff")
    match debugEnabled with
    | true -> printfn "%s: %s" ts msg
    | _ -> ()

let debugFileList (files: list<File>) =
    match debugEnabled with
    | true ->
        files
        |> List.rev
        |> List.iter (fun f ->
        for i = 0 to (int(f.size) - 1) do
            printf "%d" f.id
        )
        printfn ""
    | _ -> ()

// In part 1 the parsing assembles the data into a format that works in that case but I think in part 2 we'd be
// better served by just having data blocks with their size and starting location. There's a bit more effort
// involved in finding free spaces, but I think it should make maintaining blocks that don't move easier (since
// their address will be absolute rather than relative to all other blocks before them).
// That means that parseInput stays the same but we perform another pass over the data to determine the addresses.
// This is to avoid a mutating variable to keep track of the current block address.
let parseInput (filename: string) =
    File.ReadAllLines filename
    |> Array.head // there's just a single line of input
    |> Seq.chunkBySize 2 // one data block and one free block, always in pairs
    |> Seq.indexed
    |> Seq.map (fun (index, c) ->
        // last element can be a lone data element
        match c.Length with
        | 2 -> 
            seq { Data({id=index; address=0; size=System.Int32.Parse(c[0].ToString())}); Free({id=0; address=0; size=System.Int32.Parse(c[1].ToString())}) }
        | _ ->
            seq { Data({id=index; address=0; size=System.Int32.Parse(c[0].ToString())}) }
    )
    |> Seq.collect id // flatten
    |> Seq.toList

// I think to make this work it is easiest to split the data and free blocks into their own lists,
// so both need to be returned as a tuple.
[<TailCall>]
let rec splitBlocks (currentAddress: int) (splitted: list<Block>*list<Block>) (origBlocks: list<Block>): (list<Block>*list<Block>) =
    let head = List.head origBlocks
    let tail = List.tail origBlocks

    let dataBlocks = fst splitted
    let freeBlocks = snd splitted

    // this is a bit shitty
    let size =
        match head with
        | Data d -> d.size
        | Free f -> f.size

    let newBlocks =
        match head with
        | Data d ->
            let dataBlock = Data{id=d.id; address=currentAddress; size = d.size}
            (dataBlock :: dataBlocks, freeBlocks)
        | Free f ->
            let freeBlock = Free{id=0; address=currentAddress; size = f.size}
            (dataBlocks, freeBlock :: freeBlocks)

    match List.length tail with
    | 0 ->
        (List.rev (fst newBlocks), List.rev (snd newBlocks))
    | _ ->
        let nextAddress = currentAddress + size
        splitBlocks nextAddress newBlocks tail

let shrinkOrRemoveFreeBlock (free: File) (size: int) (freeBlocks: list<Block>): list<Block> =
    let originalSize = free.size
    let newFreeSize = originalSize - size
    let shrinkage = originalSize - newFreeSize // need to move the address forward by this amount
    let freeIndex =
        freeBlocks
        |> List.findIndex (fun b ->
            let blockAddress =
                match b with
                | Data d -> d.address
                | Free f -> f.address
            blockAddress = free.address
        )

    let newFreeBlock = Free{id=0; address=free.address+shrinkage; size=newFreeSize}
    // printfn "shrinked free block: id:%d address:%d oldSize:%d newSize:%d" freeIndex (free.address+shrinkage) originalSize newFreeSize

    match newFreeSize with
    | 0 ->
        List.removeAt freeIndex freeBlocks
    | _ ->
        let (left, right) = List.splitAt freeIndex freeBlocks
        let newRight = List.tail right
        (List.append left (newFreeBlock :: newRight))

let moveDataBlock (nextBlock: Block) (freeBlocks: list<Block>): (File*list<Block>) =
    let data = 
        match nextBlock with
        | Data d -> d
        | Free f -> f

    let matchingFreeBlock =
        freeBlocks
        // might need to index it here so we can remove later
        |> List.map (fun b ->
            let blockSize =
                match b with
                | Data d -> d.size
                | Free f -> f.size
            if blockSize >= data.size then
                Some b
            else None
        )
        |> List.tryPick id

    match matchingFreeBlock with
    | None -> // couldn't find anywhere to put it
        ({id=data.id; address=data.address; size=data.size}, freeBlocks)
    | Some f ->
        let free =
            match f with
            | Data d -> d
            | Free f -> f

        let newFile = {id=data.id; address=free.address; size=data.size}
        let newFreeBlocks = shrinkOrRemoveFreeBlock free data.size freeBlocks

        // move the data block to the free block
        (newFile, newFreeBlocks)


// Since I'm trying to keep everything immutable it means we can't just move in place which would make things easier.
// Some possible solutions:
// 1. Defragged files in one list, remaining blocks in another list, and the blocks that stayed where they are in
//    yet another list. For the third list we need to ensure that we replace moved blocks with free blocks so things
//    maintain their position.
// 2. Do away with free blocks and just maintain starting positions of data blocks. Arguably much simpler in terms of
//    what data we track. But then we can't map over free blocks, and need to take two blocks off the front of the list
//    to find how much free space there is.
// 2a. Actually we try to move data blocks to the left-most open space. I have a feeling we might need to keep a list
//     of free blocks, which then might need to be aggregated together once things move, since then we'll have empty
//     spaces of different sizes adjacent to each other. Or maybe not? Once we've moved past a data block, we won't need
//     to fill that space... so we can ignore freed space.
//
// This approach does mean we'll have to return the list of files and also the list of unmoved blocks from this
// function so we can keep them separate. Everything should be ordered by address anyway and we can sort it into
// correct order externally.
[<TailCall>]
let rec defrag (files: list<File>) (inputBlocks: list<Block>*list<Block>): list<File> =
    let dataBlocks = fst inputBlocks
    let freeBlocks = snd inputBlocks

    match List.length dataBlocks with
    | 0 -> files
    | _ ->
        // take the last block off the list
        let nextBlock = List.last dataBlocks

        // find a new place for it (which may be the same place) and update free blocks
        let (movedFile,newFreeBlocks) = moveDataBlock nextBlock freeBlocks

        // add it to the file list
        let newFiles = movedFile :: files

        // remove the block from the data block list
        let newDataBlocks = List.removeAt (List.length dataBlocks - 1) dataBlocks

        // rinse and repeat
        defrag newFiles (newDataBlocks, newFreeBlocks)

let rec makeChecksum (files: List<File>) =
    let head = List.head files
    let tail = List.tail files

    // figure out how much we need to multiply the ID by, given the start block
    // and end block numbers for the file. It's the old "sum numbers 1...100" thing.
    let blockCount = head.size
    let startBlock = head.address
    let endBlock = startBlock + blockCount - 1
    let blockMultiplier = int64((float(startBlock) + float(endBlock)) * (float(blockCount) / 2.0))
    let blockChecksum = blockMultiplier * int64(head.id)

    debug $"blocks: {startBlock}-{endBlock}; {blockMultiplier}*{head.id}; checksum: {blockChecksum}"

    match List.length tail with
    | 0 -> blockChecksum
    | _ -> blockChecksum + makeChecksum tail

let solve (filename: string): unit =
    let checksum =
        parseInput filename
        |> splitBlocks 0 (List.empty, List.empty)
        |> defrag List.empty
        |> List.sortBy (fun f -> f.address)
        // |> List.map (fun f ->
        //     printfn "id:%d address:%d size:%d" f.id f.address f.size
        //     f
        // )
        |> makeChecksum

    printfn "%d" checksum
