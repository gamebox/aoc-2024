module [
    processInput,
    compact,
    checksum,
]

Block : [
    Free,
    File U64,
]

processInput : Str -> List Block
processInput = \str ->
    dbg "processInput"
    bytes = Str.toUtf8 (Str.trim str)
    blocks = List.withCapacity (List.len bytes)
    List.walkWithIndex bytes (blocks, 0) \(bs, id), byte, index ->
        byteValue =
            Str.fromUtf8 [byte]
            |> Result.withDefault "???"
            |> Str.toU64
            |> \r ->
                when r is
                    Err _ ->
                        dbg { byte, r }
                        r

                    _ ->
                        r
            |> Result.withDefault Num.maxU64

        if Num.isEven index then
            (List.concat bs (List.repeat (File id) byteValue), id + 1)
        else if byteValue > 0 then
            (List.concat bs (List.repeat Free byteValue), id)
        else
            (bs, id)
    |> .0

blockIsFree = \block ->
    when block is
        Free -> Bool.true
        _ -> Bool.false

compact : List Block -> List Block
compact = \blocks ->
    when List.findLast blocks \b -> !(blockIsFree b) is
        Ok (File id) ->
            compactHelp blocks id

        _ -> []

FindFileState : [NoFile, FileFound U64 U64 U64]
FindSlotState : [NoSlot, Slot U64 U64]

compactHelp = \blocks, fileId ->
    # High level look for a file with fileId at the end
    findFile : FindFileState, Block, U64 -> [Continue FindFileState, Break FindFileState]
    findFile = \state, block, index ->
        when (state, block) is
            (NoFile, Free) -> Continue state
            (NoFile, File id) if fileId == id ->
                Continue (FileFound id index 1)
            (NoFile, File _) -> Continue state
            (FileFound foundId idx s, File id) if foundId == id ->
                Continue (FileFound id idx (s + 1))
            (FileFound foundId idx s, File _) -> Break state 
            (FileFound foundId idx s, Free) -> Break state
    nextFile = 
        List.walkWithIndexUntil blocks NoFile findFile
    newBlocks = 
        when nextFile is
            FileFound fId fileIndex fileSize ->
                
                # See if there is someplace (from the start) where it can fit
                findSlot : FindSlotState, Block, U64 -> [Continue FindSlotState, Break FindSlotState]
                findSlot = \state, block, index ->
                    when (state, block) is
                        (NoSlot, Free) ->
                            if fileSize == 1 then
                                Break (Slot index 1)
                            else
                                Continue (Slot index 1)
                        (Slot start size, Free) ->
                            newSize = size + 1
                            if newSize == fileSize then
                                # If there is, find it's bounds then put it where it fits and replace it's old bounds with Frees
                                Break (Slot start newSize)
                            else
                                Continue (Slot start newSize)
                        (NoSlot, File _) -> Continue NoSlot
                        (Slot _ _, File _) -> Continue NoSlot

                slot =
                    List.walkWithIndexUntil blocks NoSlot findSlot                                    
                when slot is
                    NoSlot ->
                        blocks
                    Slot slotIndex _ if slotIndex < fileIndex ->
                        fileMoved = 
                            List.walk (List.range {start: At slotIndex, end: Length fileSize}) blocks \bs, i ->
                                List.replace bs i (File fId) |> .list
                        List.walk (List.range {start: At fileIndex, end: Length fileSize}) fileMoved \bs, i ->
                            List.replace bs i Free |> .list
                    Slot _ _ ->
                        blocks
            NoFile ->
                blocks

     
    if fileId > 0 then
        compactHelp newBlocks (fileId - 1)
    else
        newBlocks


displayCompacted = \blocks ->
    List.map blocks \b ->
        when b is
            File id -> "$(Num.toStr id)"
            Free -> "."
    |> Str.joinWith ""

checksum : List Block -> U64
checksum = \blocks ->
    List.walkWithIndex blocks 0 \sum, block, index ->
        when block is
            File id -> sum + (id * index)
            Free -> sum

# got
# 009981118882777333644655556
# 0099811188827773336446555566..............
# 00992111777.44.333....5555.6666.....8888.. 

# got
# 00...111...2...333.44.5555.6666.777.888899
# 00...111...2...333.44.5555.6666.777.888899
