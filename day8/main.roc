app [main] {
    pl: platform "https://github.com/roc-lang/basic-cli/releases/download/0.17.0/lZFLstMUCUvd5bjnnpYromZJXkQUrdhbva4xdBInicE.tar.br",
}

import pl.Stdout
import pl.Utc
import "input.txt" as realInput : Str
import PartOne

getTotal = \str ->
    map = PartOne.processInput str
    PartOne.findAllObstacles map
    |> Set.len

getPart2Total = \str ->
    map = PartOne.processInput str
    PartOne.findAllObstaclesWithHarmonics map
    |> Set.len

main =
    now = Utc.now! {}
    p1Start = Utc.toNanosSinceEpoch now
    total = getTotal realInput
    p1End =
        Utc.now! {}
        |> Utc.toNanosSinceEpoch
    part2 = getPart2Total realInput
    p2End =
        Utc.now! {}
        |> Utc.toNanosSinceEpoch
    Stdout.line! "There are $(Num.toStr total) obstacles, and $(Num.toStr part2) with harmonics."
    Stdout.line! "Part 1: $(Num.toStr (p1End - p1Start))ns"
    Stdout.line! "Part 2: $(Num.toStr (p2End - p1End))ns"

expect
    input =
        """
        ............
        ........0...
        .....0......
        .......0....
        ....0.......
        ......A.....
        ............
        ............
        ........A...
        .........A..
        ............
        ............
        """
    actual = getTotal input
    actual2 = getPart2Total input
    actual == 14 && actual2 == 34

# ##....#....#
# .#.#....0...
# ..#.#0....#.
# ..##...0....
# ....0....#..
# .#...#A....#
# ...#..#.....
# #....#.#....
# ..#.....A...
# ....#....A..
# .#........#.
# ...#......##
