app [main] {
    pl: platform "https://github.com/roc-lang/basic-cli/releases/download/0.17.0/lZFLstMUCUvd5bjnnpYromZJXkQUrdhbva4xdBInicE.tar.br",
}

import pl.Stdout
import pl.File


parseLine : (List U8) -> Result (Str, Str) _
parseLine = \bytes ->
    str = Str.fromUtf8? bytes
    {before, after} = Str.splitFirst? str "   "
    Ok (before, Str.trim after)

processLine : File.Reader -> (Summary -> Task [Done Summary, Step Summary] _)
processLine = \reader -> \{listOne, listTwo} ->
    when File.readLine reader |> Task.result! is
        Ok bytes if List.len bytes == 0 ->
            Task.ok (Done {listOne, listTwo})
        Ok bytes ->
            when parseLine bytes is
                Ok (first, second) ->
                    Task.ok (Step {listOne: List.append listOne first, listTwo: List.append listTwo second})
                Err e ->
                    Task.err e
        Err _ ->
            Task.err AnError

Summary : { listOne : List Str, listTwo : List Str }

makeIntList : (List Str) -> (List I64)
makeIntList = \list -> List.map list (\n -> Str.toI64 n |> Result.withDefault 0)

incrementEntry : Dict I64 I64, I64 -> Dict I64 I64
incrementEntry = \map, n ->
    Dict.update map n \entry ->
        when entry is
            Ok value -> Ok (value + 1)
            Err _ -> Ok 1

calculateSimilarity : List I64, List I64 -> I64
calculateSimilarity = \listOne, listTwo ->
    occurrenceMap = List.walk listOne (Dict.empty {}) \map, n -> Dict.insert map n 0
    completedMap = List.walk listTwo occurrenceMap incrementEntry 
    similarity = List.walk listOne 0 \acc, n ->
        v = Dict.get completedMap n |> Result.withDefault 0
        acc + (n * v)

    similarity



main : Task {} _
main =
    reader = File.openReader! "./day-1-puzzle-1input.txt"
    readSummary = Task.loop!
            { listOne: [], listTwo: [] }
            (processLine reader)
    listOne = makeIntList readSummary.listOne
    listTwo = makeIntList readSummary.listTwo
    similarity = calculateSimilarity listOne listTwo
    Stdout.line! "Got a summary with $(Num.toStr (List.len readSummary.listOne)) lines and the similarity is $(Num.toStr similarity)"


testDataStr = Str.trim """
                       3   4
                       4   3
                       2   5
                       1   3
                       3   9
                       3   3
                       """

testData : (List Str, List Str)
testData = List.walk (Str.splitOn testDataStr "\n") ([], []) \(f, s), l -> 
    when parseLine (Str.toUtf8 l) is
        Ok (first, second) -> (List.append f first, List.append s second)
        Err _ -> (f, s)

expect
    similarity = (calculateSimilarity (testData.0 |> makeIntList) (testData.1 |> makeIntList))
    similarity == 31
