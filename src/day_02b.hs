
data Direction = Forward Int | Down Int | Up Int deriving Show
data Position = Position {horizontal :: Int, vertical :: Int, aim :: Int}


readDirection :: String -> Direction
readDirection line = case words line of
    ["forward", k] -> Forward (read k)
    ["down", k] -> Down (read k)
    ["up", k] -> Up (read k)
    _ -> error "Unknown instruction."


calcFinalProduct :: [Direction] -> Int
calcFinalProduct instructions =
    let go :: Position -> [Direction] -> Int
        go (Position h v _) [] = h * v
        go (Position h v a) (instruction : rest) =
            case instruction of
                Forward k -> go (Position (h + k) (v + a * k) a) rest
                Down k -> go (Position h v (a + k)) rest
                Up k -> go (Position h v (a - k)) rest
    in go (Position 0 0 0) instructions


main :: IO()
main = do
    content <- fmap lines (readFile "./resources/input_02.txt")
    let instructions = map readDirection content
    print $ calcFinalProduct instructions
