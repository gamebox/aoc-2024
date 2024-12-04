module [
    Matrix,
    Word,
    find,
    fromListOfList,
]

Word : (U8, U8, U8)
Loc : { x : U64, y : U64 }
Entry := (Word, Loc)
Matrix := List Entry

fromListOfList : List (List U8) -> Matrix
fromListOfList = \list ->
    width =
        List.map list \inner -> List.len inner
        |> List.min
        |> Result.withDefault 0
    height = List.len list

    heightRange = List.range { start: At 0, end: Before height }
    widthRange = List.range { start: At 0, end: Before width }

    empty : List Entry
    empty = List.withCapacity (width * height * 2)

    walkRows : List Entry, U64 -> List Entry
    walkRows = \entries, y ->
        walkColumns : List Entry, U64 -> List Entry
        walkColumns = \es, x ->
            # calcuate fits condition
            fitsRight = (x + 1) < width
            fitsLeft = x > 0
            fitsDown = (y + 1) < height
            fitsUp = y > 0
            fits = fitsRight && fitsLeft && fitsDown && fitsUp
            dbg ({x, y}, fitsUp, fitsLeft, fitsDown, fitsRight, fits)

            if !fits then
                es
            else
                # get the chars
                center = alwaysGet list x y
                tl = alwaysGet list (x - 1) (y - 1)
                tr = alwaysGet list (x + 1) (y - 1)
                bl = alwaysGet list (x - 1) (y + 1)
                br = alwaysGet list (x + 1) (y + 1)

                # build the words
                left = (tl, center, br)
                right = (tr, center, bl)
                rightBackward = (bl, center, tr)
                loc = { x : x, y : y }
                dbg (left == right, left == rightBackward)
                if (left == right || left == rightBackward) then
                    # add the entry
                    List.append es (@Entry (right, loc))
                else
                    es

        List.walk widthRange entries walkColumns

    matrixEntries = List.walk heightRange empty walkRows 


    @Matrix matrixEntries

find : Matrix, Word -> List Entry
find = \@Matrix entries, word ->
    (first, second, third) = word
    backward = (third, second, first)

    List.keepIf entries \@Entry (w, _) ->
        w == word || w == backward


alwaysGet : List (List U8), U64, U64 -> U8
alwaysGet = \origList, x, y ->
    List.get origList y
    |>Result.withDefault []
    |> List.get x
    |> Result.withDefault 0

