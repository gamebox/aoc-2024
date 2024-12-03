app [main] {
    pl: platform "https://github.com/roc-lang/basic-cli/releases/download/0.17.0/lZFLstMUCUvd5bjnnpYromZJXkQUrdhbva4xdBInicE.tar.br",
}

import pl.Stdout
import  "./day-2-puzzle-1input.txt" as realInput : Str

Report : List I64
Summary : List Report
Direction : [ Inc, Dec ]
Safety : [ Safe, Unsafe ]
Option a: [Some a, None]
LevelContext : (Option I64, Option Direction)

main : Task {} _
main =
    readSummary = processInput (Str.trim realInput)
    (safeCount, processed) = calculateSafeReportCount readSummary
    expect processed == (List.len readSummary)
    Stdout.line! "Got a summary with $(Num.toStr processed) reports and the safe report count is $(Num.toStr safeCount)"

processInput = \str ->
    List.walk (Str.splitOn str "\n") [] \summ, l -> 
        report = parseLine l
        List.append summ report

parseLine : (Str) -> (List I64)
parseLine = \str ->
    Str.splitOn str " "
    |> makeIntList


makeIntList : (List Str) -> (List I64)
makeIntList = \list -> List.map list (\n -> Str.toI64 n |> Result.withDefault 0)

ratingParams : I64, I64 -> (Direction, Bool)
ratingParams = \v, level ->
    diff = level - v
    expectedDir = if diff < 0 then Dec else Inc
    absDiff = Num.abs diff
    tooMuch = absDiff > 3
    tooLittle = absDiff == 0
    outOfBounds = tooMuch || tooLittle
    expect outOfBounds || (absDiff == 1 || absDiff == 2 || absDiff == 3)
    (expectedDir, outOfBounds)

rateLevel : LevelContext, I64 -> Result LevelContext _
rateLevel = \(prev, dir), level ->
    when (prev, dir) is
        (Some v, Some d) ->
            (expectedDir, oob) = ratingParams v level
            matchesDir = expectedDir == d
            if matchesDir && !oob then
                Ok (Some level, Some d)
            else
                Err {}

        (Some v, None) ->
            (expectedDir, oob) = ratingParams v level
            if oob then
               Err {}
            else
                newDir = Some expectedDir
                Ok (Some level, newDir)

        (None, _) ->
            Ok (Some level, dir)

isReportSafe : Report -> Safety
isReportSafe = \report ->
    when List.walkTry report (None, None) rateLevel is
        Ok _ -> Safe
        Err _ -> Unsafe

calculateSafeReportCount : Summary -> (U64, U64)
calculateSafeReportCount = \summary ->
   List.walk summary (0, 0) \(acc, processed), report ->
        when isReportSafe report is
            Safe  -> 
                (acc + 1, processed + 1)
            Unsafe ->
                (acc, processed + 1)

testDataStr = Str.trim """
                       7 6 4 2 1 0
                       1 2 7 8
                       9 7 6 2 1
                       1 3 2 5
                       8 6 4 4 1
                       1 3 6 7 9
                       """

expect
    testData = processInput testDataStr 
    (safeCount, p) = calculateSafeReportCount testData
    safeCount == 2 && p == 6
