
slidingWindow :: [Int] -> [(Int, Int, Int)]
slidingWindow (a : b : c : xs) = (a, b, c) : slidingWindow (b : c : xs)
slidingWindow _ = []


nrIncreasedDepths :: [Int] -> Int
nrIncreasedDepths depths =
    let sums = map (\(a, b, c) -> a + b + c) $ slidingWindow depths
        go :: Int -> [Int] -> Int
        go acc [] = acc
        go acc [_] = acc
        go acc (a : b : xs) =
            let acc' = if b > a then acc + 1 else acc
            in go acc' (b : xs)
    in go 0 sums


main :: IO()
main = do
    content <- fmap lines (readFile "./resources/input_01.txt")
    let depths = map read content
    print $ nrIncreasedDepths depths
