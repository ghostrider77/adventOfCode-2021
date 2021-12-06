import Data.List.Split (splitOn)
import Data.IntMap (IntMap, (!))
import qualified Data.IntMap.Strict as M

type State = IntMap Integer


readState :: [String] -> State
readState content =
    let states = map read $ splitOn  "," $ head content
        empty = M.fromList $ map (\k -> (k, 0)) [0..8]
    in foldl (\counts state -> M.insertWith (+) state 1 counts) empty states


nextDay :: State -> State
nextDay states =
    let states' = M.fromList $ map (\k -> (k, states ! (k + 1))) [0..7]
        states'' = M.insert 8 (states ! 0) states'
    in M.insertWith (+) 6 (states ! 0) states''


simulate :: State -> Int -> Integer
simulate initialState days =
    let finalState = foldl (\state _ -> nextDay state) initialState [1..days]
    in M.foldl (+) 0 finalState


main :: IO ()
main = do
    content <- fmap lines (readFile "./resources/input_06.txt")
    let initialstates = readState content
    let days = 256
    print $ simulate initialstates days
