app [main] {
    pl: platform "https://github.com/roc-lang/basic-cli/releases/download/0.17.0/lZFLstMUCUvd5bjnnpYromZJXkQUrdhbva4xdBInicE.tar.br",
}

import pl.Stdout
import "./day-3-puzzle-1input.txt" as realInput : Str

main =
    muls = processInput (Str.trim realInput)
    total = calculateTotal muls
    Stdout.line! "Total: $(Num.toStr total)"

Mul : {
    x : I64,
    y : I64,
}

ParseState : [
    LookingForOpStart,
    BuildingMulStartM,
    BuildingMulStartMU,
    BuildingDoOrDontStartD,
    BuildingDoOrDontStartDO,
    BuildingDontStartDON,
    BuildingDontStartDONQ,
    ExpectingMulOpen,
    ExpectingDoClose,
    ExpectingDontOpen,
    ExpectingDontClose,
    ExpectingX,
    BuildingX I64,
    ExpectingY I64,
    BuildingY I64 I64,
]

OpState : [
    Enabled,
    Disabled,
]

handleChar : (List Mul, ParseState, OpState), U8 -> (List Mul, ParseState, OpState)
handleChar = \(muls, state, opState), char ->
    when (state, char, opState) is
        (LookingForOpStart, 'm', s) ->
            (muls, BuildingMulStartM, s)

        (LookingForOpStart, 'd', s) ->
            (muls, BuildingDoOrDontStartD, s)

        (BuildingMulStartM, 'u', s) ->
            (muls, BuildingMulStartMU, s)

        (BuildingMulStartMU, 'l', s) ->
            (muls, ExpectingMulOpen, s)

        (BuildingDoOrDontStartD, 'o', s) ->
            (muls, BuildingDoOrDontStartDO, s)

        (BuildingDoOrDontStartDO, d, s) if d == 40 ->
            (muls, ExpectingDoClose, s)

        (ExpectingDoClose, d, _) if d == 41 ->
            (muls, LookingForOpStart, Enabled)

        (BuildingDoOrDontStartDO, d, s) if d == 'n' ->
            (muls, BuildingDontStartDON, s)

        (BuildingDontStartDON, d, s) if d == '\'' ->
            (muls, BuildingDontStartDONQ, s)

        (BuildingDontStartDONQ, d, s) if d == 't' ->
            (muls, ExpectingDontOpen, s)

        (ExpectingDontOpen, d, s) if d == 40 ->
            (muls, ExpectingDontClose, s)

        (ExpectingDontClose, d, _) if d == 41 ->
            (muls, LookingForOpStart, Disabled)

        (ExpectingMulOpen, d, s) if d == 40 ->
            (muls, ExpectingX, s)

        (ExpectingX, d, s) if (isDigit d) ->
            num = numFromDigit d
            ps = BuildingX num
            (muls, ps, s)

        (BuildingX num, d, s) if (isLessThreeDigits d num) ->
            x = addDigit num d
            ps = BuildingX x
            (muls, ps, s)

        (BuildingX num, d, s) if d == 44 && (isNoMoreThanThreeDigits num) ->
            ps = ExpectingY num
            (muls, ps, s)

        (ExpectingY x, d, s) if (isDigit d) ->
            num = numFromDigit d
            ps = BuildingY x num
            (muls, ps, s)

        (BuildingY x num, d, s) if (isLessThreeDigits d num) ->
            y = addDigit num d
            ps = BuildingY x y
            (muls, ps, s)

        (BuildingY x num, d, s) if s == Enabled && d == 41 && (isNoMoreThanThreeDigits num) ->
            mul = { x: x, y: num }
            ms = List.append muls mul
            (ms, LookingForOpStart, Enabled)

        (_, _, s) ->
            (muls, LookingForOpStart, s)

numFromDigit = \d ->
    # 48 is 0
    Num.toI64 (d - 48)

addDigit = \num, d ->
    # 48 is 0
    (num * 10) + (Num.toI64 (d - 48))

isDigit = \d ->
    # 48 is 0 and 57 is 9
    ((d >= 48) && (d <= 57))

isLessThreeDigits = \d, num ->
    # 48 is 0 and 57 is 9
    ((d >= 48) && (d <= 57)) && (num < 100)

isNoMoreThanThreeDigits = \num ->
    num < 1000

processInput = \str ->
    clean =
        Str.trim str
        |> Str.toUtf8
    state = ([], LookingForOpStart, Enabled)
    (muls, _, _) = List.walk clean state handleChar
    muls

calculateTotal = \muls ->
    List.walk muls 0 \acc, { x, y } ->
        acc + (x * y)

