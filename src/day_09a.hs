import Data.Char (digitToInt)
import Data.Map.Strict (Map, (!))
import qualified Data.Map.Strict as M


type Coord = (Int, Int)
type HeightMap = Map Coord Int


readInputData :: [String] -> HeightMap
readInputData content =
    let heights = map (map digitToInt) content
        items = [((ix, jy), x) | (row, ix) <- zip heights [1..], (x, jy) <- zip row [1..]]
    in M.fromList items


cellNeighbours :: Coord -> [Coord]
cellNeighbours (ix, jy) = [(ix + 1, jy), (ix - 1, jy), (ix, jy + 1), (ix, jy - 1)]


isLowPoint :: Coord -> HeightMap -> Bool
isLowPoint (ix, jy) heights =
    let h = heights ! (ix, jy)
        neighbours = map (\p -> M.findWithDefault 9 p heights) $ cellNeighbours (ix, jy)
    in all (> h) neighbours


sumOfRiskLevels :: HeightMap -> Int
sumOfRiskLevels heights =
    let lowPoints = M.filterWithKey (\p _ -> isLowPoint p heights) heights
    in M.foldl (\acc h -> acc + h + 1) 0 lowPoints


main :: IO ()
main = do
    content <- fmap lines (readFile "./resources/input_09.txt")
    let heights = readInputData content
    print $ sumOfRiskLevels heights
