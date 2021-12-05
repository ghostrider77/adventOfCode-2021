import Data.List.Split (splitOn)
import Data.Map.Strict (Map, (!))
import qualified Data.Map.Strict as M


type Coord = (Int, Int)
data Segment = Segment Coord Coord deriving Show


readSegments :: [String] -> [Segment]
readSegments content =
    let readSegment :: String -> Segment
        readSegment line =
            case map read $ concatMap (splitOn ",") $ splitOn " -> " line of
                [x1, y1, x2, y2] -> Segment (x1, y1) (x2, y2)
                _ -> error "Malformed input."
    in map readSegment content


isVertical :: Segment -> Bool
isVertical (Segment (x1, y1) (x2, y2)) = x1 == x2


isHorizontal :: Segment -> Bool
isHorizontal (Segment (x1, y1) (x2, y2)) = y1 == y2


orderedPoints :: Int -> Int -> (Int, Int)
orderedPoints a b = (min a b, max a b)


calcNrOverlappedPoints :: [Segment] -> Int -> Int
calcNrOverlappedPoints segments k =
    let go :: [Segment] -> Map Coord Int -> Int
        go [] counts = M.foldl (\acc cnt -> if cnt >= k then acc + 1 else acc) 0 counts
        go (segment @ (Segment (x1, y1) (x2, y2)) : rest) counts
            | isHorizontal segment =
                let (a, b) = orderedPoints x1 x2
                    counts' = foldl (\acc c -> M.insertWith (+) (c, y1) 1 acc) counts [a..b]
                in go rest counts'
            | isVertical segment =
                let (a, b) = orderedPoints y1 y2
                    counts' = foldl (\acc c -> M.insertWith (+) (x1, c) 1 acc) counts [a..b]
                in go rest counts'
            | otherwise = go rest counts
    in go segments M.empty


main :: IO ()
main = do
    content <- fmap lines (readFile "./resources/input_05.txt")
    let segments = readSegments content
    let k = 2
    print $ calcNrOverlappedPoints segments k
