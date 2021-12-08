import Data.List (elemIndex, find, permutations, sort)
import Data.List.Split (splitOn)
import Data.Map.Strict ((!))
import Data.Maybe (mapMaybe)
import qualified Data.Map.Strict as M


originalOrder :: String
originalOrder = "abcdefg"


originalDigits :: [String]
originalDigits = ["abcefg", "cf", "acdeg", "acdfg", "bcdf", "abdfg", "abdefg", "acf", "abcdefg", "abcdfg"]


readSignals :: String -> ([String], [String])
readSignals line =
    case splitOn " | " line of
        [patterns, output] ->
            let signalPatterns = words patterns
                outputValues = words output
            in (signalPatterns, outputValues)
        _ -> error "Malformed input."


decode :: String -> [String] -> [String]
decode p encodedStrings =
    let mapping = M.fromList $ zip p originalOrder
    in  map (sort . map (mapping !)) encodedStrings


isPermutationCorrect :: [String] -> String -> Bool
isPermutationCorrect encodedDigits perm =
    let decodedDigits = decode perm encodedDigits
    in all (`elem` decodedDigits) originalDigits


calcSumOfDigitsInOutput ::  [([String], [String])] -> Int
calcSumOfDigitsInOutput examples =
    let perms = permutations originalOrder
        go :: Int -> [([String], [String])] -> Int
        go acc [] = acc
        go acc ((patterns, output) : rest) =
            case find (isPermutationCorrect patterns) perms of
                Nothing -> error "Inconsistent permutation of segments."
                Just p ->
                    let decodedOutputs = decode p output
                        indices = mapMaybe (`elemIndex` originalDigits) decodedOutputs
                        displayedNumber = foldl (\n d -> n * 10 + d) 0 indices
                    in go (acc + displayedNumber) rest
    in go 0 examples


main :: IO ()
main = do
    content <- fmap lines (readFile "./resources/input_08.txt")
    let examples = map readSignals content
    print $ calcSumOfDigitsInOutput examples
