app [main] {
    pl: platform "https://github.com/roc-lang/basic-cli/releases/download/0.17.0/lZFLstMUCUvd5bjnnpYromZJXkQUrdhbva4xdBInicE.tar.br",
}

import pl.Stdout
import pl.Utc
import "input.txt" as realInput : Str
import PartOne
import PartTwo

getPartOne = \str ->
    PartOne.processInput str
    |> PartOne.compact
    |> PartOne.checksum

getPartTwo = \str ->
    PartTwo.processInput str
    |> PartTwo.compact
    |> PartTwo.checksum

main =
    now = Utc.now! {}
    p1Start = Utc.toNanosSinceEpoch now
    partOne = getPartOne realInput
    p1End =
        Utc.now! {}
        |> Utc.toNanosSinceEpoch
    partTwo = getPartTwo realInput
    p2End =
        Utc.now! {}
        |> Utc.toNanosSinceEpoch
    Stdout.line! "Part 1: $(Num.toStr partOne) [$(Num.toStr (p1End - p1Start))ns]"
    Stdout.line! "Part 2: $(Num.toStr partTwo) [$(Num.toStr (p2End - p1End))ns]"

expect
    input = "2333133121414131402"
    actual = getPartOne input
    actual == 1928

expect
    input = "2333133121414131402"
    actual = getPartTwo input
    actual == 2858
