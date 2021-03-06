
nrIncreasedDepths :: [Int] -> Int
nrIncreasedDepths depths =
    let go :: Int -> [Int] -> Int
        go acc (a : b : dss) =
            let acc' = if b > a then acc + 1 else acc
            in go acc' (b : dss)
        go acc _ = acc
    in go 0 depths


main :: IO()
main = do
    content <- fmap lines (readFile "./resources/input_01.txt")
    let depths = map read content
    print $ nrIncreasedDepths depths
