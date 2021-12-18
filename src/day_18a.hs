data SnailFishNumber = Literal Int | Pair SnailFishNumber SnailFishNumber


instance Show SnailFishNumber where
    show (Literal n) = show n
    show (Pair a b) = "[" ++ show a ++ ", " ++ show b ++ "]"


parseInput :: String -> SnailFishNumber
parseInput string =
    let go :: String -> [SnailFishNumber] -> SnailFishNumber
        go [] [] = error "Malformed input."
        go [] (result : _) = result
        go (x : xs) stack
            | x == '[' || x == ',' = go xs stack
            | x ==  ']' =
                case stack of
                    n : m : stack' -> go xs (Pair m n : stack')
                    _ -> error "Inconsistent bracket."
            | otherwise =
                let (a, b) = span (\c -> c /= ',' && c /= ']') (x : xs)
                in go b (Literal (read a) : stack)
    in go string []


calcDepth :: SnailFishNumber -> Int
calcDepth (Literal _) = 0
calcDepth (Pair n m) = 1 + max (calcDepth n) (calcDepth m)


canExplode :: SnailFishNumber -> Bool
canExplode number = calcDepth number >= 5


getLiteralValues :: SnailFishNumber -> [Int]
getLiteralValues (Literal n) = [n]
getLiteralValues (Pair n m) = getLiteralValues n ++ getLiteralValues m


canSplit :: SnailFishNumber -> Bool
canSplit number = any (>= 10) $ getLiteralValues number


explode :: SnailFishNumber -> SnailFishNumber
explode n =
    let addLeft :: SnailFishNumber -> Int -> SnailFishNumber
        addLeft (Literal m) v = Literal (m + v)
        addLeft (Pair l r) v = Pair (addLeft l v) r
        addRight :: SnailFishNumber -> Int -> SnailFishNumber
        addRight (Literal m) v = Literal (m + v)
        addRight (Pair l r) v = Pair l (addRight r v)
        go :: SnailFishNumber -> Int -> Maybe (SnailFishNumber, (Int, Int))
        go (Literal n) _ = Nothing
        go (Pair (Literal n) (Literal m)) 4 = Just (Literal 0, (n, m))
        go (Pair n m) depth =
            case go n (depth + 1) of
                Just (n', (a, b)) -> Just (Pair n' (addLeft m b), (a, 0))
                _ -> case go m (depth + 1) of
                    Just (m', (a, b)) -> Just (Pair (addRight n a) m', (0, b))
                    _ -> Nothing
    in case go n 0 of
        Just (result, _) -> result
        _ -> error "Number cannot be exploded"


split :: SnailFishNumber -> SnailFishNumber
split (Literal n) = if n < 10 then Literal n else Pair (Literal (n `div` 2)) (Literal (ceiling (fromIntegral n / 2)))
split (Pair n m) = if canSplit n then Pair (split n) m else Pair n (split m)


reduce :: SnailFishNumber -> SnailFishNumber
reduce n
    | canExplode n = reduce $ explode n
    | canSplit n = reduce $ split n
    | otherwise = n


magnitude :: SnailFishNumber -> Int
magnitude (Literal n) = n
magnitude (Pair n m) = 3 * magnitude n + 2 * magnitude m


summation :: [SnailFishNumber] -> Int
summation [] = 0
summation (n : ns) = magnitude $ foldl (\acc k -> reduce $ Pair acc k) (reduce n) ns


main :: IO()
main = do
    content <- fmap lines (readFile "./resources/input_18.txt")
    let numbers = map parseInput content
    print $ summation numbers
