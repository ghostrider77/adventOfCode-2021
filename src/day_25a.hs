import Data.Array (Array, (!), (//), array, bounds, indices)

data Cell = East | South | Empty deriving Eq

type Coord = (Int, Int)

instance Show Cell where
    show East = ">"
    show South = "v"
    show Empty = "."


readCell :: Char -> Cell
readCell '>' = East
readCell 'v' = South
readCell '.' = Empty
readCell _ = error "Unknown cell."


readInput :: [String] -> Array Coord Cell
readInput content =
    let nrRows = length content
        nrCols = length $ head content
        matrix = [((ix, jy), readCell chr) | (row, ix) <- zip content [0..], (chr, jy) <- zip row [0..]]
    in array ((0, 0), (nrRows - 1, nrCols - 1)) matrix


rightCoord :: Coord -> Int -> Coord
rightCoord (i, j) yMax = (i, if j == yMax then 0 else j + 1)


downCoord :: Coord -> Int -> Coord
downCoord (i, j) xMax = (if i == xMax then 0 else i + 1, j)


singleStep :: Array Coord Cell -> Maybe (Array Coord Cell)
singleStep seafloor =
    let (_, (xMax, yMax)) = bounds seafloor
        canMoveRight :: Array Coord Cell -> Coord -> Bool
        canMoveRight matrix (i, j) = matrix ! (i, j) == East && matrix ! rightCoord (i, j) yMax == Empty
        canMoveDown :: Array Coord Cell -> Coord -> Bool
        canMoveDown matrix (i, j) = matrix ! (i, j) == South && matrix ! downCoord (i, j) xMax == Empty
        ixs1 = filter (canMoveRight seafloor) $ indices seafloor
        rightNeighbours = map (`rightCoord` yMax) ixs1
        seafloor' = seafloor // [((i, j), Empty) | (i, j) <- ixs1] // [((i, j), East) | (i, j) <- rightNeighbours]
        ixs2 = filter (canMoveDown seafloor') $ indices seafloor'
        downNeighbours = map(`downCoord` xMax) ixs2
    in if null ixs1 && null ixs2 then Nothing
       else Just $ seafloor' // [((i, j), Empty) | (i, j) <- ixs2] // [((i, j), South) | (i, j) <- downNeighbours]


cucumberMovements :: Array Coord Cell -> Int
cucumberMovements seafloor =
    let go :: Int -> Array Coord Cell -> Int
        go nrMoves seafloor =
            case singleStep seafloor of
                Nothing -> nrMoves + 1
                Just seafloor' -> go (nrMoves + 1) seafloor'
    in go 0 seafloor


main :: IO()
main = do
    content <- fmap lines (readFile "./resources/input_25.txt")
    let seafloor = readInput content
    print $ cucumberMovements seafloor
