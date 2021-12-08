import Data.List.Split (splitOn)


readSignals :: String -> ([String], [String])
readSignals line =
    case splitOn " | " line of
        [patterns, output] ->
            let signalPatterns = words patterns
                outputValues = words output
            in (signalPatterns, outputValues)
        _ -> error "Malformed input."


calcNrEasyDigitsInOutput ::  [([String], [String])] -> Int
calcNrEasyDigitsInOutput examples =
    let easyLengths = [2, 3, 4, 7]
        go :: Int -> [([String], [String])] -> Int
        go acc [] = acc
        go acc ((_, output) : rest) =
            let nrEasyDigits = length $ filter (\o -> length o `elem` easyLengths) output
            in go (acc + nrEasyDigits) rest
    in go 0 examples


main :: IO ()
main = do
    content <- fmap lines (readFile "./resources/input_08.txt")
    let examples = map readSignals content
    print $ calcNrEasyDigitsInOutput examples
