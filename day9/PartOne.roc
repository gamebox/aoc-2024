module [
    processInput,
    compact,
    checksum,
]

Block : [
    Free U64,
    File U64 U64,
]

processInput : Str -> List Block
processInput = \str ->
    dbg "processInput"
    bytes = Str.toUtf8 (Str.trim str)
    blocks = List.withCapacity (List.len bytes)
    List.walkWithIndex bytes blocks \bs, byte, index ->
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
            List.append bs (File (index // 2) byteValue)
        else if byteValue > 0 then
            List.append bs (Free byteValue)
        else
            bs

blockIsFree = \block ->
    when block is
        Free _ -> Bool.true
        _ -> Bool.false

compact : List Block -> List Block
compact = \blocks ->
    compactHelp blocks [] 0

compactHelp = \blocks, compacted, freed ->
    dbg "compactHelp"
    if List.isEmpty blocks then
        compacted
        |> List.append (Free freed)
    else
        frontResult =
            List.walkUntil blocks ([], Err NoFreeSpace) \(acc, freeResult), b ->
                if !(blockIsFree b) then
                    Continue (List.append acc b, freeResult)
                else
                    Break (acc, Ok b)
        when frontResult is
            (nextFiles, Ok (Free freeSize)) ->
                newBlocks =
                    blocks
                    |> List.dropFirst (List.len nextFiles)
                newCompacted =
                    compacted
                    |> List.concat nextFiles
                lastFileIndex =
                    List.findLastIndex newBlocks \b -> !(blockIsFree b)
                    |> Result.withDefault Num.maxU64
                if lastFileIndex == Num.maxU64 then
                    newCompacted
                else
                    fromEnd = (List.len newBlocks) - (lastFileIndex + 1) + 1
                    dbg { lastFileIndex, fromEnd }
                    when List.get newBlocks lastFileIndex is
                        Ok (File id fileSize) ->
                            if fileSize == freeSize then
                                additionalFreed =
                                    List.takeLast newBlocks fromEnd
                                    |> List.walk 0 \sum, b ->
                                        when b is
                                            Free s ->
                                                dbg "before"
                                                sum + s

                                            _ -> sum

                                nb =
                                    newBlocks
                                    |> List.dropLast fromEnd
                                    |> List.dropFirst 1
                                nc = List.append newCompacted (File id fileSize)
                                dbg "before 3"
                                compactHelp nb nc (freed + freeSize + additionalFreed)
                            else if fileSize < freeSize then
                                additionalFreed =
                                    List.takeLast newBlocks fromEnd
                                    |> List.walk 0 \sum, b ->
                                        when b is
                                            Free s ->
                                                dbg "before 1"
                                                dbg { sum, s }
                                                sum + s

                                            _ -> sum
                                dbg "before 6"
                                nb =
                                    newBlocks
                                    |> List.replace 0 (Free (freeSize - fileSize))
                                    |> .list
                                    |> List.dropLast fromEnd
                                nc = List.append newCompacted (File id fileSize)
                                dbg "before 2"
                                dbg { freed, fileSize, additionalFreed }
                                compactHelp nb nc (freed + fileSize + additionalFreed)
                            else
                                nb =
                                    newBlocks
                                    |> List.dropFirst 1
                                dbg "before 5"
                                nbb =
                                    nb
                                    |> List.replace (lastFileIndex - 1) (File id (fileSize - freeSize))
                                    |> .list
                                nc = List.append newCompacted (File id freeSize)
                                dbg "before 4"
                                compactHelp nbb nc (freed + freeSize)

                        Ok (Free _) ->
                            crash "Got a Free when expecting a file"

                        Err _ ->
                            newCompacted
                            |> List.append (Free freed)

            (nextFiles, Ok _) -> crash "Got a File when expecting a free"
            (nextFiles, Err _) ->
                newCompacted = List.concat compacted nextFiles
                List.append newCompacted (Free freed)

displayCompacted = \blocks ->
    List.map blocks \b ->
        when b is
            File id size ->
                Str.repeat (Num.toStr id) (Num.toU64 size)

            Free size -> Str.repeat "." (Num.toU64 size)
    |> Str.joinWith ""

checksum : List Block -> U64
checksum = \blocks ->
    List.walk blocks (0, 0) \(sum, index), block ->
        dbg { sum, index }
        when block is
            File id sz ->
                size = Num.toU64 sz
                s1 =
                    List.range { start: At 0, end: Length size }
                    |> List.walk sum \s, i ->
                        ni = index + i
                        newS =
                            s + (ni * id)
                        newS

                newIndex = index + size
                (s1, newIndex)

            Free size -> (sum, index + (Num.toU64 size))
    |> .0

# got
# 009981118882777333644655556
# 0099811188827773336446555566..............

# got
# 00...111...2...333.44.5555.6666.777.888899
# 00...111...2...333.44.5555.6666.777.888899
