import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Vector (Vector, fromList, (!))

data Pixel = Light | Dark deriving Eq

type Coord = (Int, Int)


readPixel :: Char -> Pixel
readPixel c = case c of
    '#' -> Light
    '.' -> Dark
    _ -> error "Unknown pixel."


pixelsToInt :: [Pixel] -> Int
pixelsToInt ps =
    let pixelToDigit :: Pixel -> Int
        pixelToDigit Dark = 0
        pixelToDigit Light = 1
    in foldl (\acc b -> 2*acc + b) 0 $ map pixelToDigit ps


readAlgorithm :: String -> Vector Pixel
readAlgorithm line = fromList $ map readPixel line


readInputImage :: [String] -> Map Coord Pixel
readInputImage xs =
    let rows = map (map readPixel) xs
    in M.fromList [ ((ix, jy), p) | (row, ix) <- zip rows [1..], (p, jy) <- zip row [1..]]


nrLightPixels :: Map Coord Pixel -> Int
nrLightPixels image =
    let update :: Int -> Pixel -> Int
        update acc Light = acc + 1
        update acc Dark = acc
    in M.foldl update 0 image


enhanceImage :: Map Coord Pixel -> Vector Pixel -> Int -> Map Coord Pixel
enhanceImage image algorithm step =
    let getNeighbours :: Coord -> [Coord]
        getNeighbours (i, j) = [(i + dx, j + dy) | dx <- [-1..1], dy <- [-1..1]]
        calcPixel :: Coord -> Pixel
        calcPixel center =
            let neighbours = getNeighbours center
                defaultPixel = if algorithm ! 0 == Light && even step then Light else Dark
                pixels = map (\n -> M.findWithDefault defaultPixel n image) neighbours
                encoding = pixelsToInt pixels
            in algorithm ! encoding
        (((xMin, yMin), _), ((xMax, yMax), _)) = (M.findMin image, M.findMax image)
    in M.fromList [((ix, jy), calcPixel (ix, jy)) | ix <-[(xMin-1)..(xMax+1)], jy <- [(yMin-1)..(yMax+1)]]


runAlgorithm :: Map Coord Pixel -> Vector Pixel -> Int -> Int
runAlgorithm image algorithm nrRounds = nrLightPixels $ foldl (`enhanceImage` algorithm) image [1..nrRounds]


main :: IO()
main = do
    content <- fmap lines (readFile "./resources/input_20.txt")
    let algorithm = readAlgorithm $ head content
    let image = readInputImage $ drop 2 content
    print $ runAlgorithm image algorithm 50
