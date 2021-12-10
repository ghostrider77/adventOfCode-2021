import Data.Maybe (mapMaybe)
import Data.Map.Strict (Map, (!))
import qualified Data.Map.Strict as M


openingBrackets :: [Char]
openingBrackets = ['(', '[', '{', '<']


scores :: Map Char Int
scores = M.fromList [(')', 3), (']', 57), ('}', 1197), ('>', 25137)]


doBracketsMatch :: Char -> Char -> Bool
doBracketsMatch openingBracket closingBracket =
    (openingBracket == '(' && closingBracket == ')') ||
    (openingBracket == '[' && closingBracket == ']') ||
    (openingBracket == '{' && closingBracket == '}') ||
    (openingBracket == '<' && closingBracket == '>')


firstCorruptCharacter :: String -> Maybe Char
firstCorruptCharacter line =
    let go :: [Char] -> [Char] -> Maybe Char
        go stack [] = Nothing
        go stack (c : cs)
            | c `elem` openingBrackets = go (c : stack) cs
            | otherwise =
                case stack of
                    [] -> Just c
                    c' : rest ->
                        if doBracketsMatch c' c then go rest cs
                        else Just c
    in go [] line


calcIllegalCharacterScore :: [String] -> Int
calcIllegalCharacterScore xs =
    let illegalCharacters = mapMaybe firstCorruptCharacter xs
    in foldl (\acc c -> acc + scores ! c) 0 illegalCharacters


main :: IO ()
main = do
    content <- fmap lines (readFile "resources/input_10.txt")
    print $ calcIllegalCharacterScore content
