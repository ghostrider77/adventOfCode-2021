import Data.List (sort)
import Data.List.Split (splitOn)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import qualified Data.Set as S

type Rules = Map (Char, Char) Char
type PairCounts = Map (Char, Char) Int


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


insertion :: PairCounts -> Rules -> PairCounts
insertion pairCounts rules =
    let update :: PairCounts -> (Char, Char) -> Int -> PairCounts
        update newPairCounts (a, b) cnt =
            case M.lookup (a, b) rules of
                Nothing -> newPairCounts
                Just middle ->
                    let m = M.insertWith (+) (a, middle) cnt newPairCounts
                    in M.insertWith (+) (middle, b) cnt m
    in M.foldlWithKey update M.empty pairCounts


calcCountsFromPairs :: PairCounts -> Map Char Int
calcCountsFromPairs pairCounts =
    let update :: (Map Char Int, Map Char Int) -> (Char, Char) -> Int -> (Map Char Int, Map Char Int)
        update (m1, m2) (a, b) cnt = (M.insertWith (+) a cnt m1, M.insertWith (+) b cnt m2)
        (startCounts, endCounts) = M.foldlWithKey update (M.empty, M.empty) pairCounts
        commonChars = S.union (M.keysSet startCounts) (M.keysSet endCounts)
    in M.fromSet (\c -> max (M.findWithDefault 0 c startCounts) (M.findWithDefault 0 c endCounts)) commonChars


polymerSimulation :: String -> Rules -> Int -> Int
polymerSimulation polymer rules nrSteps =
    let pairs = slidingWindow polymer
        pairCounts = foldl (\acc p -> M.insertWith (+) p 1 acc) M.empty pairs
        pairCounts' = foldl (\acc _ -> insertion acc rules) pairCounts [1..nrSteps]
        counts = sort $ map snd $ M.toList $ calcCountsFromPairs pairCounts'
        largest = last counts
        smallest = head counts
    in if head polymer == last polymer then largest - smallest - 1 else largest - smallest


main :: IO()
main = do
    content <- fmap lines (readFile "./resources/input_14.txt")
    let (polymer, rules) = readData content
    let nrSteps = 40
    print $ polymerSimulation polymer rules nrSteps
