import Data.List (intercalate)
import Data.List.Split (splitOn)
import Data.Set (Set)
import qualified Data.Set as S

type Coord = (Int, Int)

data Instruction = Instruction { axis :: String, value :: Int } deriving Show


readInputData :: [String] -> (Set Coord, [Instruction])
readInputData content =
    let readLine :: String -> Coord
        readLine line = case map read $ splitOn "," line of
            [x, y] -> (x, y)
            _ -> error "Malformed input."
        readInstruction :: String -> Instruction
        readInstruction line = case splitOn "=" $ last $ words line of
            [a, v] -> Instruction { axis = a, value = read v }
            _ -> error "Malformed input."
        (firstPart, secondPart) = span (/= "") content
        coordinates = S.fromList $ map readLine firstPart
        instructions = map readInstruction $ drop 1 secondPart
    in (coordinates, instructions)


foldPaper :: Set Coord -> Instruction -> Set Coord
foldPaper paper (Instruction axis value) =
    let upward :: Coord -> Int -> Coord
        upward (x, y) v = if y > v then (x, v - (y - v)) else (x, y)
        right :: Coord -> Int -> Coord
        right (x, y) v = if x > v then (v - (x - v), y) else (x, y)
    in if axis == "y" then S.map (`upward` value) $ S.filter (\(_, y) -> y /= value) paper
    else S.map (`right` value) $ S.filter (\(x, _) -> x /= value) paper


printCode :: Set Coord -> String
printCode paper =
    let xMax = S.findMax $ S.map (\(x, y) -> x) paper
        yMax = S.findMax $ S.map (\(x, y) -> y) paper
        points = map (\y -> map (\x -> if S.member (x, y) paper then "#" else "." ) [0..xMax]) [0..yMax]
    in intercalate "\n" $ map (intercalate "") points


findCode :: Set Coord -> [Instruction] -> String
findCode points instructions = printCode $ foldl foldPaper points instructions


main :: IO ()
main = do
    content <- fmap lines (readFile "./resources/input_13.txt")
    let (points, instructions) = readInputData content
    putStrLn $ findCode points instructions
