import Data.List (sort)
import Data.Maybe (mapMaybe)
import Data.Map.Strict (Map, (!))
import qualified Data.Map.Strict as M


openingBrackets :: [Char]
openingBrackets = ['(', '[', '{', '<']


scores :: Map Char Int
scores = M.fromList [(')', 1), (']', 2), ('}', 3), ('>', 4)]


bracketPairs :: Map Char Char
bracketPairs = M.fromList [('(', ')'), ('[', ']'), ('{', '}'), ('<', '>')]


doBracketsMatch :: Char -> Char -> Bool
doBracketsMatch openingBracket closingBracket =
    (openingBracket == '(' && closingBracket == ')') ||
    (openingBracket == '[' && closingBracket == ']') ||
    (openingBracket == '{' && closingBracket == '}') ||
    (openingBracket == '<' && closingBracket == '>')


closeIncompleteLines :: String -> Maybe [Char]
closeIncompleteLines line =
    let go :: [Char] -> [Char] -> Maybe [Char]
        go [] [] = Nothing
        go stack [] = Just $ map (bracketPairs !) stack
        go stack (c : cs)
            | c `elem` openingBrackets = go (c : stack) cs
            | otherwise =
                case stack of
                    [] -> Nothing
                    c' : rest ->
                        if doBracketsMatch c' c then go rest cs
                        else Nothing
    in go [] line


calcCompletionScore :: String -> Int
calcCompletionScore = foldl (\acc b -> 5 * acc + scores ! b) 0


makeLinesComplete :: [String] -> Int
makeLinesComplete xs =
    let incompleteLines = mapMaybe closeIncompleteLines xs
        scores = sort $ map calcCompletionScore incompleteLines
        n = length scores
        half = n `div` 2
    in scores !! half


main :: IO ()
main = do
    content <- fmap lines (readFile "resources/input_10.txt")
    print $ makeLinesComplete content
