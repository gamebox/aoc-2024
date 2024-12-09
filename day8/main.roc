app [main] {
    pl: platform "https://github.com/roc-lang/basic-cli/releases/download/0.17.0/lZFLstMUCUvd5bjnnpYromZJXkQUrdhbva4xdBInicE.tar.br",
}

import pl.Stdout
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
    total = getTotal realInput
    part2 = getPart2Total realInput
    Stdout.line! "There are $(Num.toStr total) obstacles, and $(Num.toStr part2) with harmonics"

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
