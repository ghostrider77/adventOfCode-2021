import Data.Char (isLower)
import Data.List (group, partition, sort)
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


isSmallCave :: Cave -> Bool
isSmallCave (SmallCave _) = True
isSmallCave (LargeCave _) = False


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
    let neighbours = M.findWithDefault [] v cavemap
        isAllowed :: Path -> Bool
        isAllowed path =
            let startOne = length (filter (== SmallCave "start") path) == 1
                smallOccurrences = map length $ group $ sort $ filter isSmallCave path
                oneTwice = foldl (\acc cnt -> if cnt >= 2 then acc + cnt else acc) 0 smallOccurrences
            in startOne && oneTwice <= 2
    in filter isAllowed $ map (\n -> n : v : vs) neighbours


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
