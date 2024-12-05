app [main] {
    pl: platform "https://github.com/roc-lang/basic-cli/releases/download/0.17.0/lZFLstMUCUvd5bjnnpYromZJXkQUrdhbva4xdBInicE.tar.br",
}

import pl.Stdout
import "input.txt" as realInput : Str

RuleMap : Dict U64 (List U64)
Update : List U64

main =
    (rules, updates) = processInput realInput
    numRules = Dict.len rules
    numUpdates = List.len updates
    total = calculateMiddleTotal rules updates
    Stdout.line! "There are $(Num.toStr numRules) and $(Num.toStr numUpdates) and the total of all middle pages of valid updates is $(Num.toStr total)"

processInput : Str -> (RuleMap, List Update)
processInput = \str ->
    { before: rulesStr, after: updatesStr } =
        Str.trim str
        |> Str.splitFirst "\n\n"
        |> Result.withDefault { before: "", after: "" }

    rules =
        Str.splitOn rulesStr "\n"
        |> List.map \s ->
            { before: first, after: second } =
                Str.splitFirst s "|"
                |> Result.withDefault { before: "", after: "" }

            (
                Str.toU64 first
                |> Result.withDefault 99999999,
                Str.toU64 second
                |> Result.withDefault 99999999,
            )

    ruleMap =
        List.walk rules (Dict.withCapacity (List.len rules)) \dict, (before, after) ->
            addAfter : Result (List U64) [Missing] -> Result (List U64) _
            addAfter = \result ->
                when result is
                    Ok pagesAfter -> Ok (List.append pagesAfter after |> List.sortAsc)
                    Err Missing -> Ok ([after])

            Dict.update dict before addAfter

    updates =
        Str.splitOn updatesStr "\n"
        |> List.map \s ->
            Str.splitOn s ","
            |> List.map \n ->
                Str.toU64 n
                |> Result.withDefault 99999999

    (ruleMap, updates)

determinePageInOrder : RuleMap, Update -> (Bool, U64, U64 -> [Continue Bool, Break Bool])
determinePageInOrder = \ruleMap, update -> \valid, pageNo, index ->
        afters = Dict.get ruleMap pageNo |> Result.withDefault []
        before = List.takeFirst update index
        allGood = List.all before \num -> !(List.contains afters num)
        if allGood then
            Continue Bool.true
        else
            Break Bool.false

determineValidUpdate : RuleMap -> (Update -> Bool)
determineValidUpdate = \ruleMap -> \update ->
        List.walkWithIndexUntil update Bool.true (determinePageInOrder ruleMap update)

calculateMiddleTotal : RuleMap, List Update -> U64
calculateMiddleTotal = \ruleMap, updates ->
    validUpdates = List.keepIf updates (determineValidUpdate ruleMap)
    List.walk validUpdates 0 \total, update ->
        len = List.len update
        curr =
            if len == 0 then
                0
            else
                List.get update ((Num.divCeil (List.len update) 2) - 1)
                |> Result.withDefault 0
        total + curr

expect
    testDataStr =
        """
        47|53
        97|13
        97|61
        97|47
        75|29
        61|13
        75|53
        29|13
        97|29
        53|29
        61|53
        97|53
        61|29
        47|13
        75|47
        97|75
        47|61
        75|61
        47|29
        75|13
        53|13

        75,47,61,53,29
        97,61,53,29,13
        75,29,13
        75,97,47,61,53
        61,13,29
        97,13,75,29,47
        """
    (rules, updates) = processInput testDataStr
    numRules = Dict.len rules
    numUpdates = List.len updates
    total = calculateMiddleTotal rules updates
    numRules == 6 && numUpdates == 6 && total == 143
