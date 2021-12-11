import Data.Char (digitToInt)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Set (Set)
import qualified Data.Set as S

type Coord = (Int, Int)
type EnergyLevels = Map Coord Int


gridSize :: Int
gridSize = 10


readInputData :: [String] -> EnergyLevels
readInputData content =
    let energyLevels = map (map digitToInt) content
        items = [((ix, jy), x) | (row, ix) <- zip energyLevels [1..], (x, jy) <- zip row [1..]]
    in M.fromList items


isCellOnGrid :: Coord -> Bool
isCellOnGrid (ix, jy) = 1 <= ix && ix <= gridSize && 1 <= jy && jy <= gridSize


cellNeighbours :: Coord -> [Coord]
cellNeighbours (ix, jy) =
    let neighbours = [(ix + h1, jy + h2) | h1 <- [-1..1], h2 <- [-1..1]]
    in filter isCellOnGrid neighbours


singleStep :: EnergyLevels -> (EnergyLevels, Int)
singleStep energyLevels =
    let go :: Set Coord -> EnergyLevels -> Int -> (EnergyLevels, Int)
        go alreadyFlashed levels nrFlashes =
            case filter (`S.notMember` alreadyFlashed) $ M.keys $ M.filter (== 10) levels of
                [] -> (M.map (\c -> if c == 10 then 0 else c) levels, nrFlashes)
                newFlashes ->
                    let neighbours = concatMap cellNeighbours newFlashes
                        nrNewFlashes = length newFlashes
                        levels' = foldl (\acc p -> M.insertWith (\o n -> min (o + n) 10) p 1 acc) levels neighbours
                    in go (S.union alreadyFlashed $ S.fromList newFlashes) levels' (nrFlashes + nrNewFlashes)
        increasedLevels = M.map (+ 1) energyLevels
    in go S.empty increasedLevels 0


calcNrFlashes :: EnergyLevels -> Int -> Int
calcNrFlashes energyLevels nrSteps =
    let step :: (EnergyLevels, Int) -> (EnergyLevels, Int)
        step (levels, cnt) = let (levels', c) = singleStep levels in (levels', c + cnt)
    in snd $ foldl (\acc _ -> step acc) (energyLevels, 0) [1..nrSteps]


main :: IO ()
main = do
    content <- fmap lines (readFile "./resources/input_11.txt")
    let energyLevels = readInputData content
    let nrSteps = 100
    print $ calcNrFlashes energyLevels nrSteps
