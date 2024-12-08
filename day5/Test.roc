module [calculateMiddleTotal]

calculateMiddleTotal = \{} ->
    rulesMap : Set U64
    rulesMap = Set.empty {}
    sorter = \_a, _b -> if Set.isEmpty rulesMap then GT else LT
    fixed = List.sortWith [] sorter
    List.len fixed

expect
    calculateMiddleTotal {} == 123
