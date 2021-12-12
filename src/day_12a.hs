import Data.Char (isLower)
import Data.List (partition)
import Data.List.Split (splitOn)
import Data.Map.Strict (Map, (!))
import qualified Data.Map.Strict as M


type CaveMap = Map Cave [Cave]
type Path = [Cave]
data Cave = SmallCave String | LargeCave String deriving (Eq, Show)


instance Ord Cave where
    SmallCave s <= SmallCave t = s <= t
    SmallCave s <= LargeCave t = s <= t
    LargeCave s <= LargeCave t = s <= t
    LargeCave s <= SmallCave t = s <= t


readCaveSystem :: [String] -> CaveMap
readCaveSystem connections =
    let toCave :: String -> Cave
        toCave name = if all isLower name then SmallCave name else LargeCave name
        readEdge :: String -> [(Cave, [Cave])]
        readEdge line = case map toCave $ splitOn "-" line of
            [c1, c2] -> [(c1, [c2]), (c2, [c1])]
            _ -> error "Malformed input."
    in M.fromListWith (++) $ concatMap readEdge connections


extend :: Path -> CaveMap -> [Path]
extend [] _ = [[SmallCave "start"]]
extend (v : vs) cavemap =
    let isAllowed :: Cave -> Path -> Bool
        isAllowed (LargeCave _) _ = True
        isAllowed c path = c `notElem` path
        neighbours = filter (`isAllowed` vs) $ M.findWithDefault [] v cavemap
    in map (\n -> n : v : vs) neighbours


findAllPaths :: Cave -> CaveMap -> [Path]
findAllPaths v cavemap =
    let go :: [Path] -> [Path] -> [Path]
        go completePaths paths =
            let extendedPaths = concatMap (`extend` cavemap) paths
                (finished, unfinished) = partition (\p -> head p == SmallCave "end") extendedPaths
                completePaths' = completePaths ++ finished
            in if null unfinished then completePaths' else go completePaths' unfinished
    in go [] [[v]]


main :: IO ()
main = do
    content <- fmap lines (readFile "./resources/input_12.txt")
    let caves = readCaveSystem content
    print $ length $ findAllPaths (SmallCave "start") caves
