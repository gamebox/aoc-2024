app [main] {
    pl: platform "https://github.com/roc-lang/basic-cli/releases/download/0.17.0/lZFLstMUCUvd5bjnnpYromZJXkQUrdhbva4xdBInicE.tar.br",
}

import pl.Stdout
import pl.Utc
import "input.txt" as real_input : Str
import PartOne
import PartTwo

get_part_one = |str|
    PartOne.process_input(str)
    |> PartOne.compact
    |> PartOne.checksum

get_part_two = |str|
    PartTwo.process_input(str)
    |> PartTwo.compact
    |> PartTwo.checksum

main = |_|
    p1_start = Utc.now!().to_nanos_since_epoch()
    part_one = get_part_one(real_input)
    p1_end = Utc.now!().to_nanos_since_epoch()
    part_two = get_part_two(real_input)
    p2_end = Utc.now!().to_nanos_since_epoch()
    Stdout.line! "Part 1: $(part_one.to_str()) [$((p1_end - p1_start).toStr())ns]"
    Stdout.line! "Part 2: $(part_two.to_str()) [$((p2_end - p1_end).toStr())ns]"

expect
    input = "2333133121414131402"
    actual = get_part_one(input)
    actual == 1928

expect
    input = "2333133121414131402"
    actual = get_part_two(input)
    actual == 2858
