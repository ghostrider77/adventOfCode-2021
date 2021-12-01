
nrIncreasedDepths :: [Int] -> Int
nrIncreasedDepths depths =
    let go :: Int -> [Int] -> Int
        go acc [] = acc
        go acc [_] = acc
        go acc (a : b : dss) =
            let acc' = if b > a then acc + 1 else acc
            in go acc' (b : dss)
    in go 0 depths


main :: IO()
main = do
    content <- fmap lines (readFile "./resources/input_01.txt")
    let depths = map read content
    print $ nrIncreasedDepths depths
