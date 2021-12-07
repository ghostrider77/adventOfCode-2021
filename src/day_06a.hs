import Data.List.Split (splitOn)

newtype LanternFish = LanternFish Int


readState :: [String] -> [LanternFish]
readState content = map (LanternFish . read) $ splitOn "," $ head content


nextDay :: [LanternFish] -> [LanternFish]
nextDay fish =
    let fishStateChange :: [LanternFish] -> LanternFish -> [LanternFish]
        fishStateChange acc (LanternFish state)
            | state > 0 = LanternFish (state - 1) : acc
            | otherwise = LanternFish 8 : LanternFish 6 : acc
    in foldl fishStateChange [] fish


simulate :: [LanternFish] -> Int -> Int
simulate state days = length $ foldl (\fish _ -> nextDay fish) state [1..days]


main :: IO ()
main = do
    content <- fmap lines (readFile "./resources/input_06.txt")
    let initialstates = readState content
    let days = 80
    print $ simulate initialstates days
