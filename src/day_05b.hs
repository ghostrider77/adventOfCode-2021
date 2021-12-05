import Data.List.Split (splitOn)
import Data.Map.Strict (Map)
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


isDiagonal :: Segment -> Bool
isDiagonal (Segment (x1, y1) (x2, y2)) = abs (y2 - y1) == abs (x2 - x1)


orderedPoints :: Int -> Int -> (Int, Int)
orderedPoints a b = (min a b, max a b)


pointsOnSegment :: Segment -> [Coord]
pointsOnSegment segment
    | isHorizontal segment =
        let Segment (x1, y) (x2, _) = segment
            (a, b) = orderedPoints x1 x2
        in map (\x -> (x, y)) [a..b]
    | isVertical segment =
        let Segment (x, y1) (_, y2) = segment
            (a, b) = orderedPoints y1 y2
        in map(\y -> (x, y)) [a..b]
    | isDiagonal segment =
        let Segment (x1, y1) (x2, y2) = segment
            ((p1, p2), (q1, q2)) = if x1 <= x2 then ((x1, y1), (x2, y2)) else ((x2, y2), (x1, y1))
            xs = if q1 >= p1 then [p1..q1] else [q1..p1]
            ys = if q2 >= p2 then [p2..q2] else reverse [q2..p2]
        in zip xs ys
    | otherwise = []


calcNrOverlappedPoints :: [Segment] -> Int -> Int
calcNrOverlappedPoints segments k =
    let go :: [Segment] -> Map Coord Int -> Int
        go [] counts = M.foldl (\acc cnt -> if cnt >= k then acc + 1 else acc) 0 counts
        go (segment : rest) counts =
            let coordinates = pointsOnSegment segment
                counts' = foldl (\acc p -> M.insertWith (+) p 1 acc) counts coordinates
            in go rest counts'
    in go segments M.empty


main :: IO ()
main = do
    content <- fmap lines (readFile "./resources/input_05.txt")
    let segments = readSegments content
    let k = 2
    print $ calcNrOverlappedPoints segments k
