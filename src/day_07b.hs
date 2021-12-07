import Data.List.Split (splitOn)


readPositions :: [String] -> [Int]
readPositions content = map read $ splitOn  "," $ head content


calcAlignmentPoint :: [Int] -> Int
calcAlignmentPoint crabs =
    let distance :: Int -> Int -> Int
        distance a b = let d = abs (a - b) in d * (d + 1) `div` 2
        (minValue, maxValue) = (minimum crabs, maximum crabs)
        findBestScore :: Int -> Int -> Int
        findBestScore bestScore m =
            let score = sum $ map (`distance` m) crabs
            in if score < bestScore then score else bestScore
        in foldl findBestScore maxBound [minValue..maxValue]


main :: IO ()
main = do
    content <- fmap lines (readFile "./resources/input_07.txt")
    let crabs = readPositions content
    print $ calcAlignmentPoint crabs
