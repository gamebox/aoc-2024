app [main] {
    pl: platform "https://github.com/roc-lang/basic-cli/releases/download/0.17.0/lZFLstMUCUvd5bjnnpYromZJXkQUrdhbva4xdBInicE.tar.br",
}

import pl.Stdout
import  "./day-3-puzzle-1input.txt" as realInput : Str

Mul : { x : I64, y : I64 }
ParseState : [
    LookingForOpStart,
    BuildingMulStartM,
    BuildingMulStartMU,
    BuildingDoOrDontStartD,
    BuildingDoOrDontStartDO,
    BuildingDontStartDON,
    BuildingDontStartDON2,
    ExpectingMulOpen,
    ExpectingDoClose,
    ExpectingDontClose,
    ExpectingDontOpen,
    ExpectingX,
    BuildingX I64,
    ExpectingY I64,
    BuildingY I64 I64,
]
OpState: [Enabled, Disabled]

main : Task {} _
main =
    muls = processInput (Str.trim realInput)
    Stdout.line! "Number of Muls: $(Num.toStr (List.len muls))"
    total = calculateTotal muls
    Stdout.line! "Total: $(Num.toStr total)"

isDigit : U8 -> Bool
isDigit = \d ->
    d >= '0' && d <= '9'

handleChar : (List Mul, ParseState, OpState), U8 -> (List Mul, ParseState, OpState)
handleChar = \(muls, state, opState), char ->
    when (state, char, opState) is
        (LookingForOpStart, 'm', _) ->
            (muls, BuildingMulStartM, opState)
        (LookingForOpStart, 'd', _) ->
            (muls, BuildingDoOrDontStartD, opState)
        (BuildingMulStartM, 'u', _) ->
            (muls, BuildingMulStartMU, opState)
        (BuildingMulStartMU, 'l', _) ->
            (muls, ExpectingMulOpen, opState)
        (BuildingDoOrDontStartD, 'o', _) ->
            (muls, BuildingDoOrDontStartDO, opState)
        (BuildingDoOrDontStartDO, '(', _) ->
            (muls, ExpectingDoClose, opState)
        (ExpectingDoClose, ')', _) ->
            (muls, LookingForOpStart, Enabled)
        (BuildingDoOrDontStartDO, 'n', _) ->
            (muls, BuildingDontStartDON, opState)
        # 39 is a ' character
        (BuildingDontStartDON, 39, _) ->
            (muls, BuildingDontStartDON2, opState)
        (BuildingDontStartDON2, 't', _) ->
            (muls, ExpectingDontOpen, opState)
        (ExpectingDontOpen, '(', _) ->
            (muls, ExpectingDontClose, opState)
        (ExpectingDontClose, ')', _) ->
            (muls, LookingForOpStart, Disabled)
        (ExpectingMulOpen, '(', _) ->
            (muls, ExpectingX, opState)
        (ExpectingX, d, _) if isDigit d ->
            (muls, BuildingX (Num.toI64 (d - '0')), opState)
        (BuildingX num, d, _) if isDigit d && num < 100 ->
            (muls, BuildingX ((num * 10) + (Num.toI64 (d - '0'))), opState)
        (BuildingX num, ',', _) if num < 1000 ->
            (muls, ExpectingY num, opState)
        (ExpectingY x, d, _) if isDigit d ->
            (muls, BuildingY x (Num.toI64 (d - '0')), opState)
        (BuildingY x num, d, _) if isDigit d && num < 100 ->
            (muls, BuildingY x ((num * 10) + (Num.toI64 (d - '0'))), opState)
        (BuildingY x num, ')', Enabled) if num < 1000 ->
            (List.append muls {x: x, y: num}, LookingForOpStart, opState)
        _ ->
            (muls, LookingForOpStart, opState)


processInput : Str -> List Mul
processInput = \str ->
    clean =
        Str.trim str
        |> Str.toUtf8
    (muls, _, _) = List.walk clean ([], LookingForOpStart, Enabled) handleChar
    muls

calculateTotal : List Mul -> I64
calculateTotal = \muls ->
    List.walk muls 0 \acc, {x, y} -> acc + (x * y) 

expect
    testDataStr = Str.trim """
                           xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))
                           """
    testData = processInput testDataStr 
    total = calculateTotal testData
    total == 48

expect
    testDataStr = Str.trim """
                           mul(200,5)&don't()xxx$mul(3,4)
                           """
    testData = processInput testDataStr
    total = calculateTotal testData
    total == 1000
