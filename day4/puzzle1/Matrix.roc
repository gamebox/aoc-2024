module [
    Matrix,
    Word,
    find,
    fromListOfList,
]

Word : (U8, U8, U8, U8)
Loc : { x : U64, y : U64 }
Orientation : [
    Hor,
    Vert,
    DiagLeft,
    DiagRight,
]
Entry := (Word, Loc, Orientation)
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
            fitsRight = (x + 3) < width
            fitsLeft = x >= 3
            fitsDown = (y + 3) < height
            one = alwaysGet list x y
            withHors = 
                if fitsRight then
                    two = alwaysGet list (x + 1) y
                    three = alwaysGet list (x + 2) y
                    four = alwaysGet list (x + 3) y
                    List.append es (@Entry ((one, two, three, four), {x: x, y: y}, Hor))
                else es
            withVerts =
                if fitsDown then
                    two = alwaysGet list x (y + 1)
                    three = alwaysGet list x (y + 2)
                    four = alwaysGet list x (y + 3)
                    List.append withHors (@Entry ((one, two, three, four), {x: x, y: y}, Vert))
                else withHors
                    
            withDiagRight =
                if fitsRight && fitsDown then
                    two = alwaysGet list (x + 1) (y + 1)
                    three = alwaysGet list (x + 2) (y + 2)
                    four = alwaysGet list (x + 3) (y + 3)
                    List.append withVerts (@Entry ((one, two, three, four), {x: x, y: y}, DiagRight))
                else withVerts

            withDiagLeft =
                if fitsLeft && fitsDown then
                    two = alwaysGet list (x - 1) (y + 1)
                    three = alwaysGet list (x - 2) (y + 2)
                    four = alwaysGet list (x - 3) (y + 3)
                    List.append withDiagRight (@Entry ((one, two, three, four), {x: x, y: y}, DiagLeft))
                else withDiagRight

            withDiagLeft

        List.walk widthRange entries walkColumns

    matrixEntries = List.walk heightRange empty walkRows 


    @Matrix matrixEntries

find : Matrix, Word -> List Entry
find = \@Matrix entries, word ->
    (first, second, third, fourth) = word
    backward = (fourth, third, second, first)

    List.keepIf entries \@Entry (w, _, _) ->
        w == word || w == backward


alwaysGet : List (List U8), U64, U64 -> U8
alwaysGet = \origList, x, y ->
    List.get origList y
    |>Result.withDefault []
    |> List.get x
    |> Result.withDefault 0

