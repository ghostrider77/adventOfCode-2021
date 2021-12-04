import Data.List (find)
import Data.List.Split (splitOn)
import Data.Map.Strict (Map, (!))
import qualified Data.Map.Strict as M

type Coord = (Int, Int)
data Cell = Marked Int | UnMarked Int deriving Show
type Board = Map Coord Cell

boardSize :: Int
boardSize = 5


convertToIntlist :: String -> [Int]
convertToIntlist = map read . words


isWinning :: Board -> Bool
isWinning board =
    let isMarked :: Cell -> Bool
        isMarked (Marked _) = True
        isMarked (UnMarked _) = False
        existsMarkedRow = any (\ix -> all (\jy -> isMarked $ board ! (ix, jy)) [1..boardSize]) [1..boardSize]
        existsMarkedCol = any (\jy -> all (\ix -> isMarked $ board ! (ix, jy)) [1..boardSize]) [1..boardSize]
    in existsMarkedRow || existsMarkedCol


readInputData :: [String] -> ([Int], [Board])
readInputData content =
    let numbers = map read $ splitOn "," (head content)
        matrixToBoard :: [[Int]] -> Board
        matrixToBoard matrix =
            let items = do
                (row, ix) <- zip matrix [1..]
                (x, jy) <- zip row [1..]
                return ((ix, jy), UnMarked x)
            in M.fromList items
        readBoards :: [String] -> [Board]
        readBoards [] = []
        readBoards ls =
            let rows = map convertToIntlist $ drop 1 $ take 6 ls
            in matrixToBoard rows : readBoards (drop 6 ls)
    in (numbers, readBoards $ tail content)


updateBoard :: Board -> Int -> Board
updateBoard board number =
    let update :: Cell -> Cell
        update (UnMarked x) = if x == number then Marked x else UnMarked x
        update cell = cell
    in M.map update board


calcScore :: (Int, Board) -> Int
calcScore (n, board) =
    let update :: Int -> Cell -> Int
        update acc (UnMarked k) = acc + k
        update acc _ = acc
    in n * M.foldl update 0 board


playBingo :: [Int] -> [Board] -> Maybe Int
playBingo numbers inputboards =
    let go :: [Int] -> [Board] -> Maybe (Int, Board)
        go [] _ = Nothing
        go (n : nss) boards =
            let boards' = map (`updateBoard` n) boards
            in case find isWinning boards' of
                Nothing -> go nss boards'
                Just winningBoard -> Just (n, winningBoard)
        board = go numbers inputboards
    in fmap calcScore board


main :: IO ()
main = do
    content <- fmap lines (readFile "./resources/input_04.txt")
    let (numbers, boards) = readInputData content
    let result = playBingo numbers boards
    print result
