import Data.Char (digitToInt)
import Data.List (sortOn)
import Data.Set (Set)
import qualified Data.Set as S
import Data.Ord (Down(..))
import qualified Data.Sequence as Seq
import Data.Sequence (Seq, (><))
import Data.Map.Strict (Map, (!))
import qualified Data.Map.Strict as M

type Coord = (Int, Int)
type HeightMap = Map Coord Int


readInputData :: [String] -> HeightMap
readInputData content =
    let heights = map (map digitToInt) content
        items = [((ix, jy), x) | (row, ix) <- zip heights [1..], (x, jy) <- zip row [1..]]
    in M.fromList items


cellNeighbours :: Coord -> HeightMap -> [Coord]
cellNeighbours (ix, jy) heights = filter (`M.member` heights) [(ix + 1, jy), (ix - 1, jy), (ix, jy + 1), (ix, jy - 1)]


isLowPoint :: Coord -> HeightMap  -> Bool
isLowPoint (ix, jy) heights =
    let h = heights ! (ix, jy)
        neighbours = map (heights !) $ cellNeighbours (ix, jy) heights
    in all (> h) neighbours


findBasinSize :: Coord -> HeightMap -> Int
findBasinSize lowPoint heights =
    let go :: Set Coord -> Seq Coord -> Int
        go visited queue
            | Seq.null queue = S.size visited
            | otherwise =
                let cell = Seq.index queue 0
                    neighbours = filter (`S.notMember` visited) $ cellNeighbours cell heights
                    basinNeighbours = filter (\c -> heights ! c /= 9) neighbours
                    queue' = Seq.drop 1 queue >< Seq.fromList basinNeighbours
                    visited' = S.union visited (S.fromList basinNeighbours)
                in go visited' queue'
    in go (S.singleton lowPoint) (Seq.fromList [lowPoint])


productRiskLevelsInLargeBasins :: HeightMap -> Int
productRiskLevelsInLargeBasins heights =
    let lowPoints = M.keys $ M.filterWithKey (\p _ -> isLowPoint p heights) heights
        basinSizes = map (`findBasinSize` heights) lowPoints
    in product $ take 3 $ sortOn Down basinSizes


main :: IO ()
main = do
    content <- fmap lines (readFile "./resources/input_09.txt")
    let heights = readInputData content
    print $ productRiskLevelsInLargeBasins heights
