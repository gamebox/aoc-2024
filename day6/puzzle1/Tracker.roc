module [Tracker, fromStr, allPositions, obstaclesThatCreateLoop]

Point : { x : I64, y : I64 }
TrackingData : [
    Empty,
    Obstacle,
]
Direction : [Up, Left, Down, Right]
Tracker := { width : U64, height : U64, data : List TrackingData, guardPosition : (Point, Direction) }

fromStr : Str -> Tracker
fromStr = \str ->
    rows =
        Str.splitOn str "\n"
        |> List.map (Str.toUtf8)
    height = List.len rows
    expect height > 0
    width = List.len (List.first rows |> Result.withDefault [])
    createDataAndGuardPosition = \state, bytes, x ->
        List.walkWithIndex bytes state \(trackingData, guard), c, y ->
            when c is
                '.' | 'X' -> (List.append trackingData Empty, guard)
                '#' -> (List.append trackingData Obstacle, guard)
                '^' if guard == None -> (List.append trackingData Empty, Some ({ x: Num.toI64 x, y: Num.toI64 y }, Up))
                '>' if guard == None -> (List.append trackingData Empty, Some ({ x: Num.toI64 x, y: Num.toI64 y }, Left))
                'v' if guard == None -> (List.append trackingData Empty, Some ({ x: Num.toI64 x, y: Num.toI64 y }, Down))
                '<' if guard == None -> (List.append trackingData Empty, Some ({ x: Num.toI64 x, y: Num.toI64 y }, Right))
                _ ->
                    crash "Bad input, char is $(Str.fromUtf8 [c] |> Result.withDefault "😂") and guard was already set = $(if guard == None then "True" else "False")"

    when List.walkWithIndex rows (List.withCapacity (height * width), None) createDataAndGuardPosition is
        (data, Some guardPosition) ->
            @Tracker { width, height, data, guardPosition }

        (_, None) ->
            crash "Expected to find a guard, but was missing from input"

fromParams : { width : U64, height : U64, data : List TrackingData, guardPosition : (Point, Direction) } -> Tracker
fromParams = \params ->
    @Tracker params

moveGuardHelp : Tracker, List Point, List Point, List (List Point) -> Result (List Point) [Cycle]
moveGuardHelp = \t, positions, currentSegment, segments ->
    when nextPosition t NoTurn is
        Ok (next, point, Turns _) ->
            if List.contains segments currentSegment then
                Err Cycle
            else
                poss = List.concat positions currentSegment
                ss = List.append segments currentSegment
                moveGuardHelp next poss [point] ss

        Ok (next, point, NoTurn) ->
            moveGuardHelp next positions (List.append currentSegment point) segments

        Err OffMapArea ->
            Ok positions

        Err Cycle ->
            Err Cycle

moveGuard : Tracker, List Point -> Result (List Point) [Cycle]
moveGuard = \t, positions ->
    moveGuardHelp t positions positions []

allPositions : Tracker -> Result (List Point) [Cycle]
allPositions = \@Tracker params ->
    { guardPosition: (start, _) } = params
    moveGuard (@Tracker params) [start]

Turning : [NoTurn, Turns U8]

addTurn = \t ->
    when t is
        NoTurn -> Turns 1
        Turns n -> Turns (n + 1)

nextPointInDir = \p, dir ->
    when dir is
        Up -> { x: p.x - 1, y: p.y }
        Left -> { x: p.x, y: p.y + 1 }
        Down -> { x: p.x + 1, y: p.y }
        Right -> { x: p.x, y: p.y - 1 }

nextPosition : Tracker, Turning -> Result (Tracker, Point, Turning) [OffMapArea, Cycle]
nextPosition = \@Tracker { width, height, data, guardPosition: (p, dir) }, turning ->
    next = nextPointInDir p dir
    if offMapArea next height width then
        Err OffMapArea
    else if turning == Turns 4 then
        Err Cycle
    else
        index = (Num.toU64 next.x) * width + (Num.toU64 next.y)
        when List.get data index is
            Ok Obstacle ->
                nextPosition (@Tracker { width, height, data, guardPosition: (p, turnRight dir) }) (addTurn turning)

            Ok Empty ->
                Ok (@Tracker { width, height, data, guardPosition: (next, dir) }, next, turning)

            Err _ ->
                crash "Somehow we are out of bounds when we thought we weren't next is { x: $(Num.toStr next.x), y: $(Num.toStr next.y) } and index is $(Num.toStr index)"

offMapArea : Point, U64, U64 -> Bool
offMapArea = \{ x, y }, height, width ->
    x < 0 || x >= (Num.toI64 height) || y < 0 || y >= (Num.toI64 width)

listScan : List a, (a, a -> b) -> List b
listScan = \list, scanner ->
    when list is
        [] | [_] -> []
        [first, .. as rest] ->
            walker = \(l, prev), item ->
                (List.append l (scanner prev item), item)
            empty = List.withCapacity ((List.len rest) - 1)

            List.walk rest (empty, first) walker
            |> .0

turnRight : Direction -> Direction
turnRight = \dir ->
    when dir is
        Up -> Left
        Left -> Down
        Down -> Right
        Right -> Up

withGuardPosition : Tracker, Point, Direction -> Tracker
withGuardPosition = \@Tracker tracker, p, d ->
    @Tracker { tracker & guardPosition: (p, d) }

addObstacle : Tracker, Point -> Tracker
addObstacle = \@Tracker tracker, p ->
    obstacleIndex = tracker.width * (Num.toU64 p.x) + (Num.toU64 p.y)
    @Tracker { tracker & data: List.set tracker.data obstacleIndex Obstacle }

resetGuard : Tracker, Point, Direction -> Tracker
resetGuard = \@Tracker tracker, p, dir ->
    @Tracker { tracker & guardPosition: (p, dir) }

obstaclesThatCreateLoop : Tracker, List Point -> List Point
obstaclesThatCreateLoop = \t, pointList ->
    (@Tracker { guardPosition, height, width }) = t
    (startingPoint, startingDir) = guardPosition
    moves = List.walk (List.range { start: At 0, end: Before height }) [] \acc, a ->
        ms = List.map (List.range { start: At 0, end: Before width }) \b ->
            { x: Num.toI64 a, y: Num.toI64 b }
        List.concat acc ms
    (obstacles, _) =
        List.walk moves ([], startingDir) \(os, dir), p1 ->

            tracker =
                t
                |> addObstacle p1
                |> withGuardPosition startingPoint startingDir

            when allPositions tracker is
                Err Cycle ->
                    (List.append os p1, dir)

                _ ->
                    (os, dir)

    obstacles

display : Tracker, Set Point -> Str
display = \@Tracker { width, height, data, guardPosition }, positions ->
    (guardPoint, guardDir) = guardPosition
    rows = List.chunksOf data width
    expect (List.len rows) == height
    displayRow : List TrackingData, U64 -> Str
    displayRow = \row, x ->
        displayPoint : TrackingData, U64 -> U8
        displayPoint = \d, y ->
            curr = { x: Num.toI64 x, y: Num.toI64 y }
            if guardPoint == curr then
                when guardDir is
                    Up -> '^'
                    Left -> '>'
                    Down -> 'v'
                    Right -> '<'
            else if Set.contains positions curr then
                'X'
            else
                when d is
                    Empty -> '.'
                    Obstacle -> '#'
        List.mapWithIndex row displayPoint
        |> Str.fromUtf8
        |> Result.withDefault "?????????????????"
    List.mapWithIndex rows displayRow
    |> Str.joinWith "\n"

testParams = { width: 3, height: 3, data: [Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty], guardPosition: ({ x: 1, y: 1 }, Up) }

expect
    tracker = fromParams testParams
    expected =
        """
        ...
        .^.
        ...
        """
    actual = display tracker (Set.empty {})
    actual == expected

expect
    input =
        """
        ...
        .^.
        ...
        """
    fromStr input
    |> \@Tracker expected ->
        expected == testParams

expect
    input =
        """
        .#.
        #^#
        .#.
        """
    fromStr input
    |> \t ->
        (allPositions t) == Err Cycle

expect
    input =
        """
        .#..
        .^.#
        ..#.
        """
    fromStr input
    |> \t ->
        p = \x, y -> { x, y }
        path = [p 1 1, p 1 2, p 1 1, p 1 0]
        obstacles = obstaclesThatCreateLoop t path

        obstacles == [p 1 0]
