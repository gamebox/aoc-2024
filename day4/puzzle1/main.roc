app [main] {
    pl: platform "https://github.com/roc-lang/basic-cli/releases/download/0.17.0/lZFLstMUCUvd5bjnnpYromZJXkQUrdhbva4xdBInicE.tar.br",
}

import pl.Stdout
import "input.txt" as realInput : Str
import Matrix

main =
    m = processInput realInput
    total = List.len (Matrix.find m needle)
    Stdout.line! "Total: $(Num.toStr total)"

needle = ('X', 'M', 'A', 'S')

processInput = \str ->
    clean =
        Str.trim str
        |> Str.splitOn "\n"
        |> List.map Str.toUtf8
    Matrix.fromListOfList clean

expect
    testDataStr = """
                  MMMSXXMASM
                  MSAMXMSMSA
                  AMXSXMAAMM
                  MSAMASMSMX
                  XMASAMXAMM
                  XXAMMXXAMA
                  SMSMSASXSS
                  SAXAMASAAA
                  MAMMMXMMMM
                  MXMXAXMASX
                  """
    m = processInput testDataStr
    word = needle
    results = Matrix.find m word
    total = List.len results
    total == 18

expect
    testDataStr = """
                  XMASSAMX
                  MM....MM
                  A.A..A.A
                  S..SS..S
                  S..SS..S
                  A.A..A.A
                  MM....MM
                  XMASSAMX
                  """
    m = processInput testDataStr
    word = needle
    results = Matrix.find m word
    total = List.len results
    total == 12
