app [main] {
    pl: platform "https://github.com/roc-lang/basic-cli/releases/download/0.17.0/lZFLstMUCUvd5bjnnpYromZJXkQUrdhbva4xdBInicE.tar.br",
}

import pl.Stdout
import  "./day-3-puzzle-1input.txt" as realInput : Str

Mul : { x : I64, y : I64 }
ParseState : [
    LookingForOpStart,
    BuildingMulStart Str,
    BuildingDoOrDontStart Str,
    BuildingDontStart Str,
    ExpectingMulOpen,
    ExpectingDoClose,
    ExpectingDontClose,
    ExpectingDontOpen,
    ExpectingX,
    BuildingX Str,
    ExpectingY I64,
    BuildingY I64 Str,
]
OpState: [Enabled, Disabled]

main : Task {} _
main =
    muls = processInput (Str.trim realInput)
    Stdout.line! "Number of Muls: $(Num.toStr (List.len muls))"
    total = calculateTotal muls
    Stdout.line! "Total: $(Num.toStr total)"

convert : Str -> I64
convert = \s ->
    Str.toI64 s
    |> Result.withDefault 99999999

convertNum : U8 -> Str
convertNum = \d ->
    Str.fromUtf8 [d]
    |> Result.withDefault "XXXXXXX"

isDigit : U8 -> Bool
isDigit = \d ->
    d >= '0' && d <= '9'

handleChar : (List Mul, ParseState, OpState), U8 -> (List Mul, ParseState, OpState)
handleChar = \(muls, state, opState), char ->
    when (state, char, opState) is
        (LookingForOpStart, 'm', _) ->
            (muls, BuildingMulStart "m", opState)
        (LookingForOpStart, 'd', _) ->
            (muls, BuildingDoOrDontStart "d", opState)
        (BuildingMulStart "m", 'u', _) ->
            (muls, BuildingMulStart "mu", opState)
        (BuildingMulStart "mu", 'l', _) ->
            (muls, ExpectingMulOpen, opState)
        (BuildingDoOrDontStart "d", 'o', _) ->
            (muls, BuildingDoOrDontStart "do", opState)
        (BuildingDoOrDontStart "do", '(', _) ->
            (muls, ExpectingDoClose, opState)
        (ExpectingDoClose, ')', _) ->
            (muls, LookingForOpStart, Enabled)
        (BuildingDoOrDontStart "do", 'n', _) ->
            (muls, BuildingDoOrDontStart "don", opState)
        (BuildingDoOrDontStart "don", '\'', _) ->
            (muls, BuildingDoOrDontStart "don'", opState)
        (BuildingDoOrDontStart "don'", 't', _) ->
            (muls, ExpectingDontOpen, opState)
        (ExpectingDontOpen, '(', _) ->
            (muls, ExpectingDontClose, opState)
        (ExpectingDontClose, ')', _) ->
            (muls, LookingForOpStart, Disabled)
        (ExpectingMulOpen, '(', _) ->
            (muls, ExpectingX, opState)
        (ExpectingX, d, _) if isDigit d ->
            (muls, BuildingX (convertNum d), opState)
        (BuildingX num, d, _) if isDigit d && (Str.countUtf8Bytes num) < 3 ->
            (muls, BuildingX "$(num)$(convertNum d)", opState)
        (BuildingX num, ',', _) if (Str.countUtf8Bytes num) <= 3 ->
            (muls, ExpectingY (convert num), opState)
        (ExpectingY x, d, _) if isDigit d ->
            (muls, BuildingY x (convertNum d), opState)
        (BuildingY x num, d, _) if isDigit d && (Str.countUtf8Bytes num) < 3 ->
            (muls, BuildingY x "$(num)$(convertNum d)", opState)
        (BuildingY x num, ')', Enabled) if (Str.countUtf8Bytes num) <= 3 ->
            (List.append muls {x: x, y: convert num}, LookingForOpStart, opState)
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
    num = convertNum '2'
    num2 = convertNum '1'
    num == "2" && num2 == "1"

expect
    num = convert "200"
    num2 = convert "16"
    num == 200 && num2 == 16

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
