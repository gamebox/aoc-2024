app [main] {
    pl: platform "https://github.com/roc-lang/basic-cli/releases/download/0.17.0/lZFLstMUCUvd5bjnnpYromZJXkQUrdhbva4xdBInicE.tar.br",
}

import pl.Stdout
import PartOne
import PartTwo
import "input.txt" as realInput : Str

getTotal = \input ->
    PartOne.processInput input
    |> PartOne.findSolvableEquations
    |> List.walk 0 \acc, { testValue } -> acc + testValue

getPartTwoTotal = \input ->
    PartTwo.processInput input
    |> PartTwo.findSolvableEquations
    |> List.walk 0 \acc, { testValue } -> acc + testValue

main =
    equationValue = getTotal realInput
    Stdout.line! "The answer for part 1 $(Num.toStr equationValue)."
    partTwoValue = getPartTwoTotal realInput
    Stdout.line! "The answer for part 2: $(Num.toStr partTwoValue)."

expect
    input =
        """
        190: 10 19
        3267: 81 40 27
        83: 17 5
        156: 15 6
        7290: 6 8 6 15
        161011: 16 10 13
        192: 17 8 14
        21037: 9 7 18 13
        292: 11 6 16 20
        """

    equationValue = getTotal input
    partTwoValue = getPartTwoTotal realInput

    equationValue == 3749 && partTwoValue == 11387
