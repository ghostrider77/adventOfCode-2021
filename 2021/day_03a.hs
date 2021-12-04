import Data.Char (digitToInt)
import Data.List (partition, transpose)


calcPowerConsumption :: [String] -> Int
calcPowerConsumption report =
    let mostCommon :: String -> Char
        mostCommon xs =
            let (zeroes, ones) = partition (== '0') xs
            in if length zeroes >= length ones then '0' else '1'
        binaryToDecimal :: String -> Int
        binaryToDecimal = foldl (\acc c -> 2*acc + digitToInt c) 0
        gammaRate = map mostCommon $ transpose report
        epsilonRate = map (\b -> if b == '0' then '1' else '0') gammaRate
        gamma = binaryToDecimal gammaRate
        epsilon = binaryToDecimal epsilonRate
    in gamma * epsilon


main :: IO()
main = do
    content <- fmap lines (readFile "./resources/input_03.txt")
    print $ calcPowerConsumption content
