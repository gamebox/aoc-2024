module [processInput, findSolvableEquations]

Operator : [Add, Mul]
Equation : { testValue : U64, operands : List U64 }

toU64 : Str -> U64
toU64 = \str ->
    when Str.toU64 str is
        Ok n -> n
        _ -> crash "Could not convert $(str) into a number"

processInput : Str -> List Equation
processInput = \str ->
    lines =
        Str.trim str
        |> Str.splitOn "\n"

    List.map lines \line ->
        { before: testValueStr, after: operandsStr } = Str.splitFirst line ": " |> Result.withDefault { before: "", after: "" }
        operands = Str.splitOn operandsStr " "
        { testValue: toU64 testValueStr, operands: List.map operands (toU64) }

runWithOperators : List U64, List Operator -> U64
runWithOperators = \operands, operators ->
    expect ((List.len operators) + 1) == (List.len operands)
    when operands is
        [first, .. as rest] ->
            operations = List.map2 rest operators \n, op -> (op, n)
            List.walk operations first \total, (op, n) ->
                when op is
                    Add -> total + n
                    Mul -> total * n

        _ -> crash "Invalid list of operands"

isEquationSolvable : List U64, U64 -> Bool
isEquationSolvable = \operands, testValue ->
    combos = operatorCombos ((List.len operands) - 1) [Add, Mul]
    List.any combos \combo ->
        result = runWithOperators operands combo
        testValue == result

findSolvableEquations : List Equation -> List Equation
findSolvableEquations = \equations ->
    solvableEqs = List.keepIf equations \{ testValue, operands } ->
        solvable = isEquationSolvable operands testValue
        solvable
    solvableEqs

operatorCombos : U64, List Operator -> List (List Operator)
operatorCombos = \number, ops ->
    empty : List (List Operator)
    empty = []

    if number == 0 then
        [[]]
    else
        combos = operatorCombos (number - 1) ops
        walkOps : List (List Operator), Operator -> List (List Operator)
        walkOps = \acc, op ->
            walkCombo : List (List Operator), List Operator -> List (List Operator)
            walkCombo = \all, combo ->
                newCombo : List Operator
                newCombo = List.prepend combo op
                List.prepend all newCombo

            List.walk combos acc walkCombo
        List.walk ops empty walkOps

expect
    actual = operatorCombos 2 [Add, Mul]
    (Set.fromList actual) == (Set.fromList [[Add, Add], [Add, Mul], [Mul, Mul], [Mul, Add]])

expect
    actual = runWithOperators [10, 19] [Add]
    actual == 29

expect
    actual = runWithOperators [10, 19] [Mul]
    actual == 190

expect
    actual = runWithOperators [11, 6, 16, 20] [Add, Mul, Add]
    actual == 292