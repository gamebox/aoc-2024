module [
    process_input,
    compact,
    checksum,
]

Block : [
    Free(U64),
    File(U64, U64),
]

num_from_byte_or_crash : U8 -> U64
num_from_byte_or_crash = |byte|
    when Str.fromUtf8([byte]) is
        Ok(s) ->
            when s.to_u64() is
                Ok(n) -> n
                _ -> crash "Could not convert number to U64"
            end
        _ -> crash "Could not convert $(Inspect.toStr(byte)) to a string"
    end
end

process_input : Str -> List(Block)
process_input = |str|
    bytes = str.trim().to_utf8()
    blocks = List.with_capacity(bytes.len())
    bytes.walk_with_index(blocks, |bs, byte, index|
        byte_value = num_from_byte_or_crash(byte)
        if index.is_even() then
            bs.append(File(index // 2, byte_value))
        else 
            bs.append(Free(byte_value))
        end
    end)
end

block_is_free = |block|
    when block is
        Free(_) -> Bool.true
        _ -> Bool.false
    end
end

compact : List(Block) -> List(Block)
compact = |blocks| compact_help(blocks, [], 0)

add_one_to_free = |sum, b|
    when b is
        Free(s) -> sum + s
        _ -> sum
    end
end

compact_help = |blocks, compacted, freed|
    if blocks.isEmpty() then
        compacted.append(Free(freed))
    else
        front_result =
            blocks.walk_until(([], Err(NoFreeSpace)), |(acc, free_result), b|
                if !(block_is_free(b)) then
                    Continue((acc.append(b), free_result))
                else
                    Break((acc, Ok(b)))
                end
            end)

        when front_result is
            (next_files, Ok(Free(free_size))) ->
                new_blocks = blocks.dropFirst(next_files.len())
                new_compacted = compacted.concat(next_files)
                last_file_index =
                    new_blocks
                    .find_last_index(|b| !block_is_free(b))
                    .with_default(Num.maxU64)

                if last_file_index == Num.maxU64 then
                    new_compacted
                else
                    from_end = new_blocks.len() - (last_file_index + 1) + 1
                    when new_blocks.get(last_file_index) is
                        Ok(File(id, file_size)) ->
                            if file_size == free_size then
                                additional_freed =
                                    new_blocks
                                    .take_last(from_end)
                                    .walk(0, add_one_to_free)

                                nb = 
                                    new_blocks
                                    .drop_last(from_end)
                                    .drop_first(1)
                                nf = File(id, file_size)
                                nc = new_compacted.append(nf)
                                nfreed = freed + free_size + additional_freed

                                compact_help(nb, nc, nfreed)
                            else if file_size < free_size then
                                additional_freed =
                                    new_blocks
                                    .take_last(from_end)
                                    .walk(0, add_one_to_free)

                                nfree = Free(free_size - file_size)
                                nb =
                                    new_blocks
                                    .set(0, nfree)
                                    .drop_last(from_end)
                                nfile = File(id, file_size)
                                nc = new_compacted.append(nfile)
                                nfreed = freed + free_size + additional_freed

                                compact_help(nb, nc, nfreed)
                            else
                                nfmoved = File(id, file_size - free_size)
                                nfstay = File(id, free_size)
                                nb =
                                    new_blocks
                                    .drop_first(1)
                                    .set(last_file_index - 1, nfmoved)
                                nc = new_compacted.append(newCompacted, nfstay)

                                compact_help(nb, nc, freed + free_size)
                            end

                        Err(_) -> new_compacted.append(Free(freed))
                        Ok(Free(_)) -> crash "Got a Free when expecting a file"
                    end
                end

            (next_files, Err(_)) ->
                compacted.concat(next_files).append(Free(freed))
            (next_files, Ok(_)) -> crash("Got a File when expecting a free")
        end
    end
end

displayCompacted = |blocks|
    blocks.map(|b|
        when b is
            File(id, size) -> id.to_str().repeat(size.to_u64())
            Free(size) -> ".".repeat(size.to_u64())
        end
    end).pass_to(Str.joinWith(""))
end

checksum : List(Block) -> U64
checksum = |blocks|
    var sum_ = 0
    var index_ = 0

    for block in blocks do
        when block is
            File(id, sz) ->
                for i in List.range({ start: At(0), end: Length(size) }) do
                    ni = index + i
                    sum_ = sum_ + (ni * id)
                    index_ = index_ + 1

            Free size ->
                index_ = index_ + size.to_u64()
        end
    end

    sum_
end

# got
# 009981118882777333644655556
# 0099811188827773336446555566..............

# got
# 00...111...2...333.44.5555.6666.777.888899
# 00...111...2...333.44.5555.6666.777.888899
