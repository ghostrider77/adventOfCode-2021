import Data.List (group, intercalate, sort, sortOn)
import Data.List.Split (splitOn)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Ord (Down(..))

type Rules = Map (Char, Char) Char


readData :: [String] -> (String, Rules)
readData content =
    let polymer = head content
        readRule :: String -> ((Char, Char), Char)
        readRule line = case splitOn " -> " line of
            [[a, b], [c]] -> ((a, b), c)
            _ -> error "Malformed input."
        rules = M.fromList $ map readRule $ drop 2 content
    in (polymer, rules)


slidingWindow :: [a] -> [(a, a)]
slidingWindow (x : y : xs) = (x, y) : slidingWindow (y : xs)
slidingWindow _ = []


insertion :: String -> Rules -> String
insertion polymer rules =
    let pairs = slidingWindow polymer
        triples = map (\(a, b) -> let middle = M.findWithDefault ' ' (a, b) rules in [a, middle, b]) pairs
        parts = map (\(t, ix) -> if ix == 1 then t else tail t) $ zip triples [1..]
    in filter (/= ' ') $ intercalate "" parts


polymerSimulation :: String -> Rules -> Int -> Int
polymerSimulation polymer rules nrSteps =
    let polymer' = foldl (\acc _ -> insertion acc rules) polymer [1..nrSteps]
        sizes = sortOn Down $ map length $ group $ sort polymer'
    in head sizes - last sizes


main :: IO()
main = do
    content <- fmap lines (readFile "./resources/input_14.txt")
    let (polymer, rules) = readData content
    let nrSteps = 3
    print $ polymerSimulation polymer rules nrSteps
