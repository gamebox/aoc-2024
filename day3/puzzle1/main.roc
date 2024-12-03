app [main] {
    pl: platform "https://github.com/roc-lang/basic-cli/releases/download/0.17.0/lZFLstMUCUvd5bjnnpYromZJXkQUrdhbva4xdBInicE.tar.br",
}

import pl.Stdout
import  "./day-3-puzzle-1input.txt" as realInput : Str

Mul : { x : I64, y : I64 }
ParseState : [
    LookingForStart,
    BuildingStart Str,
    ExpectingOpen,
    ExpectingX,
    BuildingX Str,
    ExpectingY I64,
    BuildingY I64 Str,
]

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

handleChar : (List Mul, ParseState), U8 -> (List Mul, ParseState)
handleChar = \(muls, state), char ->
    when (state, char) is
        (LookingForStart, 'm') ->
            (muls, BuildingStart "m")
        (BuildingStart "m", 'u') ->
            (muls, BuildingStart "mu")
        (BuildingStart "mu", 'l') ->
            (muls, ExpectingOpen)
        (ExpectingOpen, '(') ->
            (muls, ExpectingX)
        (ExpectingX, d) if isDigit d ->
            (muls, BuildingX (convertNum d))
        (BuildingX num, d) if isDigit d && (Str.countUtf8Bytes num) < 3 ->
            (muls, BuildingX "$(num)$(convertNum d)")
        (BuildingX num, ',') if (Str.countUtf8Bytes num) <= 3 ->
            (muls, ExpectingY (convert num))
        (ExpectingY x, d) if isDigit d ->
            (muls, BuildingY x (convertNum d))
        (BuildingY x num, d) if isDigit d && (Str.countUtf8Bytes num) < 3 ->
            (muls, BuildingY x "$(num)$(convertNum d)")
        (BuildingY x num, ')') if (Str.countUtf8Bytes num) <= 3 ->
            (List.append muls {x: x, y: convert num}, LookingForStart)
        _ ->
            (muls, LookingForStart)


processInput : Str -> List Mul
processInput = \str ->
    clean =
        Str.trim str
        |> Str.toUtf8
    (muls, _) = List.walk clean ([], LookingForStart) handleChar
    muls

calculateTotal : List Mul -> I64
calculateTotal = \muls ->
    List.walk muls 0 \acc, {x, y} -> acc + (x * y) 

expect
    _name = "Test convertNum"
    num = convertNum '2'
    num2 = convertNum '1'
    num == "2" && num2 == "1"

expect
    _name = "Test convert"
    num = convert "200"
    num2 = convert "16"
    num == 200 && num2 == 16

expect
    testDataStr = Str.trim """
                           xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))
                           """
    testData = processInput testDataStr 
    total = calculateTotal testData
    total == 161

expect
    testDataStr = Str.trim """
                           mul(200,5)&mul[3,4]
                           """
    testData = processInput testDataStr
    total = calculateTotal testData
    total == 1000
