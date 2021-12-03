import Data.Char (digitToInt)
import Data.List (partition, transpose)


findLastRemaining :: [String] -> (String -> Char) -> String
findLastRemaining strings criteria =
    let go :: [String] -> String -> String
        go [] acc = reverse acc
        go [lastRow] acc = reverse acc ++ lastRow
        go xs acc =
            let c = criteria $ map head xs
                xss = map (drop 1) $ filter (\row -> head row == c) xs
            in go xss (c : acc)
    in go strings []


checkLifeSupportRating :: [String] -> Int
checkLifeSupportRating report =
    let mostCommon :: String -> Char
        mostCommon xs =
            let (zeroes, ones) = partition (== '0') xs
            in if length ones >= length zeroes then '1' else '0'
        leastCommon :: String -> Char
        leastCommon xs = if mostCommon xs == '1' then '0' else '1'
        binaryToDecimal :: String -> Int
        binaryToDecimal = foldl (\acc c -> 2*acc + digitToInt c) 0
        oxygenRate = findLastRemaining report mostCommon
        co2Rate = findLastRemaining report leastCommon
        oxygen = binaryToDecimal oxygenRate
        co2 = binaryToDecimal co2Rate
    in oxygen * co2


main :: IO()
main = do
    content <- fmap lines (readFile "./resources/input_03.txt")
    print $ checkLifeSupportRating content
