import Data.Char (digitToInt)
import Data.List (transpose)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Array (Array, listArray, (!), range)

type Coord = (Int, Int)
type Risks = Array Coord Int


readInput :: [String] -> Int -> Risks
readInput content n =
    let originalRows = map (map digitToInt) content
        extend :: [Int] -> [Int]
        extend row = concatMap (\ix -> map (\x -> let r = x + ix in if r >= 10 then r - 9 else r) row) [0..4]
        columns = transpose $ map extend originalRows
        rows = transpose $ map extend columns
    in listArray ((1, 1), (5*n, 5*n)) $ concat rows


calcNeighbours :: Coord -> Int -> [Coord]
calcNeighbours (i, j) size =
    let isValid :: Coord -> Bool
        isValid (i, j) = i > 0 && i <= size && j > 0 && j <= size
    in filter isValid [(i - 1, j), (i + 1, j), (i, j + 1), (i, j - 1)]


updateDistances :: Risks -> Int -> Coord -> Int -> Map Coord Int -> Map Coord Int -> (Map Coord Int, Map Coord Int)
updateDistances matrix n (i, j) distU distances t =
    let neighbours = calcNeighbours (i, j) n
        go :: [Coord] -> Map Coord Int -> Map Coord Int -> (Map Coord Int, Map Coord Int)
        go [] distances' t' = (distances', t')
        go (v @ (vi, vj) : rest) distances' t' =
            let distanceThroughU = distU + matrix ! (vi, vj)
                (distances'', t'') =
                    if (distances' M.! v) <= distanceThroughU then (distances', t')
                    else (M.insert v distanceThroughU distances', M.insert v distanceThroughU t')
            in go rest distances'' t''
    in go neighbours distances t


shortestPath :: Risks -> Int -> Int
shortestPath matrix n =
    let (topLeft, bottomRight) = ((1, 1), (n, n))
        bounds = (topLeft, bottomRight)
        initialDistances =
            M.fromList [((i, j), if (i, j) == topLeft then matrix ! (i, j) else maxBound) | (i, j) <- range bounds]
        distances = runDijkstrasAlgorithm initialDistances initialDistances
        findNodeWithSmallestDistance :: Map Coord Int -> (Coord, Int)
        findNodeWithSmallestDistance m =
            M.foldlWithKey
                (\acc @ (minNode, minDist) node d -> if d < minDist then (node, d) else acc) ((0, 0), maxBound) m
        runDijkstrasAlgorithm :: Map Coord Int -> Map Coord Int -> Map Coord Int
        runDijkstrasAlgorithm distances t
            | M.null t = distances
            | otherwise =
                let (u, distU) = findNodeWithSmallestDistance t
                    t' = M.delete u t
                    (distances', t'') = updateDistances matrix n u distU distances t'
                in runDijkstrasAlgorithm distances' t''
    in distances M.! bottomRight - distances M.! topLeft


main :: IO()
main = do
    content <- fmap lines (readFile "./resources/input_15.txt")
    let n = length content
    let riskLevels = readInput content n
    print $ shortestPath riskLevels (5 * n)
