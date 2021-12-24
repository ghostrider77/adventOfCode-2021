import Data.List.Split (splitOn)
import Data.Set (Set)
import qualified Data.Set as S

data Instruction = On | Off
data Step = Step { instruction :: Instruction, xBounds :: (Int, Int), yBounds :: (Int, Int), zBounds :: (Int, Int) }

type Coord = (Int, Int, Int)


readInstruction :: String -> Instruction
readInstruction text = case text of
    "on" -> On
    "off" -> Off
    _ -> error "Unknown instruction."


readInput :: [String] -> [Step]
readInput content =
    let msg = "Malformed input."
        parseBounds :: String -> (Int, Int)
        parseBounds line =
            case map read $ splitOn ".." $ drop 2 line of
                [a, b] -> (a, b)
                _ -> error msg
        parseRow :: String -> Step
        parseRow line = case words line of
            [text, rest] ->
                let instruction = readInstruction text
                    parts = splitOn "," rest
                in case map parseBounds parts of
                    [xb, yb, zb] -> Step instruction xb yb zb
                    _ -> error msg
            _ -> error msg
    in map parseRow content


executeSteps :: [Step] -> Int -> Int
executeSteps steps cutoff =
    let collectCubes :: (Int, Int) -> (Int, Int) -> (Int, Int) -> Set Coord
        collectCubes (xMin, xMax) (yMin, yMax) (zMin, zMax) =
            let coordinates = do
                x <- [(max xMin (-cutoff))..(min xMax cutoff)]
                y <- [(max yMin (-cutoff))..(min yMax cutoff)]
                z <- [(max zMin (-cutoff))..(min zMax cutoff)]
                return (x, y, z)
            in S.fromAscList coordinates
        go :: Set Coord -> [Step] -> Int
        go state [] = S.size state
        go state (x : xs) =
            let points = collectCubes (xBounds x) (yBounds x) (zBounds x)
            in case instruction x of
                On -> go (S.union state points) xs
                Off -> go (S.difference state points) xs
    in go S.empty steps


main :: IO()
main = do
    content <- fmap lines (readFile "./resources/input_22.txt")
    let steps = readInput content
    let cutoff = 50
    print $ executeSteps steps cutoff
