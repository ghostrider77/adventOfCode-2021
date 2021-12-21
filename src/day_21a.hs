data Player = Player { position :: Position, score :: Int } deriving Show

type Position = Int


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


rollDie :: Int -> Int
rollDie lastRoll = lastRoll `mod` 100 + 1


turn :: Player -> Int -> (Player, Int)
turn player lastRoll =
    let r1 = rollDie lastRoll
        r2 = rollDie r1
        r3 = rollDie r2
        nextPosition = move (position player) (r1 + r2 + r3)
        nextScore = score player + nextPosition
    in (Player nextPosition nextScore, r3)


hasWon :: Player -> Bool
hasWon player = score player >= 1000


game :: Player -> Player -> Int
game p1 p2 =
    let go :: Player -> Player -> Int -> Int -> Int
        go p1 p2 nrRolls lastRoll =
            let (p1', lastRoll') = turn p1 lastRoll
                (p2', lastRoll'') = turn p2 lastRoll'
            in if hasWon p1' then score p2 * (nrRolls + 3)
            else if hasWon p2' then score p1' * (nrRolls + 6)
            else go p1' p2' (nrRolls + 6) lastRoll''
    in go p1 p2 0 0


main :: IO()
main = do
    content <- fmap lines (readFile "./resources/input_21.txt")
    let (p1, p2) = readInput content
    print $ game p1 p2
