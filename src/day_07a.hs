import Data.List.Split (splitOn)
import Data.List (sort)


readPositions :: [String] -> [Int]
readPositions content = map read $ splitOn  "," $ head content


calcAlignmentPoint :: [Int] -> Int
calcAlignmentPoint crabs =
    let xs = sort crabs
        n = length crabs
        half = n `div` 2
        m = if n `mod` 2 == 1 then xs !! half else (xs !! (half - 1) + xs !! half) `div` 2
    in sum $ map (\x -> abs (x - m)) xs


main :: IO ()
main = do
    content <- fmap lines (readFile "./resources/input_07.txt")
    let crabs = readPositions content
    print $ calcAlignmentPoint crabs
