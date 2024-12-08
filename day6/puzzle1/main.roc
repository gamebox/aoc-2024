app [main] {
    pl: platform "https://github.com/roc-lang/basic-cli/releases/download/0.17.0/lZFLstMUCUvd5bjnnpYromZJXkQUrdhbva4xdBInicE.tar.br",
}

import pl.Stdout
import "input.txt" as realInput : Str
import Tracker

main =
    (totalPositions, totalObstacles) = getNumberOfPositionsAndObstacles realInput
    Stdout.line! "The guard moved through $(Num.toStr totalPositions) unique position visited, and there are $(Num.toStr totalObstacles) places to add an obstacles to create a loop."

getNumberOfPositionsAndObstacles = \input ->
    tracker = Tracker.fromStr (Str.trim input)
    when Tracker.allPositions tracker is
        Ok positions ->
            positionSet = Set.fromList positions
            obstacles = Tracker.obstaclesThatCreateLoop tracker positions
            (Set.len positionSet, List.len obstacles)

        Err Cycle ->
            crash "Found a cycle in the input"

expect
    (positions, obstacles) =
        """
        ....#.....
        .........#
        ..........
        ..#.......
        .......#..
        ..........
        .#..^.....
        ........#.
        #.........
        ......#...
        """
        |> getNumberOfPositionsAndObstacles

    positions == 41 && obstacles == 6
