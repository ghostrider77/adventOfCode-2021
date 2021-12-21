import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M

data Player = Player { position :: Position, score :: Int } deriving (Eq, Ord, Show)

type Position = Int
type Cache = Map (Player, Player) (Integer, Integer)


readInput :: [String] -> (Player, Player)
readInput content =
    let readPlayer :: String -> Player
        readPlayer line =
            let start = read $ last $ words line
            in Player start 0
    in case map readPlayer content of
        [p1, p2] -> (p1, p2)
        _ -> error "More than 2 players are given."


move :: Position -> Int -> Position
move current steps = (current + steps - 1) `mod` 10 + 1


rollDie :: [(Int, Int, Int)]
rollDie = [(r1, r2, r3) | r1 <- [1..3], r2 <- [1..3], r3 <- [1..3]]


turn :: Player -> Int -> Int -> Int -> Player
turn player r1 r2 r3 =
    let nextPosition = move (position player) (r1 + r2 + r3)
        nextScore = score player + nextPosition
    in Player nextPosition nextScore


hasWon :: Player -> Bool
hasWon player = score player >= 21


game :: Player -> Player -> (Integer, Integer)
game player1 player2 =
    let getNrWins :: Cache -> Player -> Player -> (Cache, (Integer, Integer))
        getNrWins cache p1 p2
            | hasWon p1 = (cache, (1, 0))
            | hasWon p2 = (cache, (0, 1))
            | otherwise =
                let update :: (Cache, (Integer, Integer)) -> (Int, Int, Int) -> (Cache, (Integer, Integer))
                    update (cache, (w1, w2)) (r1, r2, r3) =
                        let p1' = turn p1 r1 r2 r3
                            (cache', (a, b)) = getNrWins cache p2 p1'
                        in (cache', (w1 + b, w2 + a))
                in case M.lookup (p1, p2) cache of
                    Just result -> (cache, result)
                    Nothing ->
                        let (cache', (w1, w2)) = foldl update (cache, (0, 0)) rollDie
                        in (M.insert (p1, p2) (w1, w2) cache', (w1, w2))
    in snd $ getNrWins M.empty player1 player2

main :: IO()
main = do
    content <- fmap lines (readFile "./resources/input_21.txt")
    let (p1, p2) = readInput content
    let (w1, w2) = game p1 p2
    print $ max w1 w2
