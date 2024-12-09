module [processInput, findAllObstacles, findAllObstaclesWithHarmonics]

Data : [Empty, Antenna U8]
Map : { height : U64, width : U64, data : List Data }
AntennaPoint : (U8, U64, U64)
Point : (U64, U64)
Delta : (I64, I64)

processInput : Str -> Map
processInput = \str ->
    lines : List (List Data)
    lines =
        Str.trim str
        |> Str.splitOn "\n"
        |> List.map \line ->
            Str.toUtf8 line
            |> List.map \c ->
                when c is
                    '.' ->
                        Empty

                    _ ->
                        Antenna c

    when lines is
        [] ->
            crash "Can't process an empty map"

        [first, ..] ->
            width = List.len first
            height = List.len lines
            data = List.join lines
            { height, width, data }

findAllObstaclesWithHarmonics = \map ->
    { height, width, data } = map
    obstacles =
        findAllObstaclesHelp map
        |> getInBoundsObstaclesWithHarmonics height width
    obstacles

findAllObstaclesHelp = \{ height, width, data } ->
    processRows = \a, row, x ->
        addAntennaeToSet = \acc, col, y ->
            when col is
                Antenna c ->
                    Set.insert acc (c, x, y)

                _ ->
                    acc

        List.walkWithIndex row a addAntennaeToSet

    List.chunksOf data width
    |> List.walkWithIndex (Set.empty {}) processRows
    |> getAllAntennaPairs width

findAllObstacles = \map ->
    { height, width } = map
    findAllObstaclesHelp map
    |> getInBoundsObstacles height width

display = \data, width, obstacles ->
    data
    |> List.chunksOf width
    |> List.mapWithIndex \line, x ->
        List.mapWithIndex line \d, y ->
            when (d, Set.contains obstacles (x, y)) is
                (Empty, hasAntinode) if hasAntinode -> '#'
                (Empty, _) -> '.'
                (Antenna c, hasAntinode) if hasAntinode -> '^'
                (Antenna c, _) -> c
        |> Str.fromUtf8
        |> Result.withDefault ""
    |> Str.joinWith "\n"

createPair = \p1, p2, width ->
    if (p1.1 * width + p1.2) < (p2.1 * width + p2.2) then
        ((p1.1, p1.2), (p2.1, p2.2))
    else
        ((p2.1, p2.2), (p1.1, p1.2))

getAllAntennaPairs : Set AntennaPoint, U64 -> Set (Point, Point)
getAllAntennaPairs = \antennae, width ->
    empty = Set.empty {}

    walkPairs = \acc, left ->
        walkPairsInner = \all, right ->
            when (left, right) is
                ((l, xl, yl), (r, xr, yr)) if l == r && (xl != xr || yl != yr) ->
                    pair = createPair left right width
                    Set.insert all (createPair left right width)

                _ ->
                    all

        Set.walk antennae acc walkPairsInner
    Set.walk antennae empty walkPairs

getDelta : Point, Point -> Delta
getDelta = \p1, p2 ->
    (x1, y1) = (Num.toI64 p1.0, Num.toI64 p1.1)
    (x2, y2) = (Num.toI64 p2.0, Num.toI64 p2.1)
    (x2 - x1, y2 - y1)

flipDelta : Delta -> Delta
flipDelta = \(x, y) -> (x * -1, y * -1)

checkedToU64 = \n ->
    if n < 0 then
        Err Underflow
    else
        Ok (Num.toU64 n)

inBoundsPoint : Point, Delta, U64, U64 -> Result Point [OutOfBounds]
inBoundsPoint = \(px, py), (dx, dy), height, width ->
    (x, y) = (Num.toI64 px, Num.toI64 py)
    when (checkedToU64 (x + dx), checkedToU64 (y + dy)) is
        (Ok x1, Ok y1) if x1 < height && y1 < width ->
            Ok (x1, y1)

        _ ->
            Err OutOfBounds

applyDeltaUnchecked = \(px, py), (dx, dy) ->
    (x, y) = (Num.toI64 px, Num.toI64 py)
    (x + dx, y + dy)

insertHarmonic = \set, point, delta, height, width ->
    when inBoundsPoint point delta height width is
        Ok p ->
            insertHarmonic (Set.insert set p) p delta height width

        _ ->
            set

getInBoundsObstacles = \pairs, height, width ->
    Set.walk pairs (Set.empty {}) \set, (p1, p2) ->
        delta = getDelta p1 p2
        flipped = flipDelta delta
        s1 =
            when inBoundsPoint p1 flipped height width is
                Ok p ->
                    Set.insert set p

                _ ->
                    set

        when inBoundsPoint p2 delta height width is
            Ok p ->
                Set.insert s1 p

            _ ->
                s1

getInBoundsObstaclesWithHarmonics = \pairs, height, width ->
    Set.walk pairs (Set.empty {}) \set, (p1, p2) ->
        delta = getDelta p1 p2
        flipped = flipDelta delta
        insertHarmonic set p1 flipped height width
        |> insertHarmonic p2 delta height width
        |> Set.insert p1
        |> Set.insert p2
