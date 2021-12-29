import Data.Char (digitToInt)
import Numeric (readHex)
import Text.Printf (printf)
import Data.Maybe (mapMaybe)


data Packet = LiteralValue { version :: Int, bitSize :: Int, value :: Int }
            | Operator { version :: Int, bitSize :: Int, packets :: [Packet] } deriving Show


readInput :: String -> String
readInput hex =
    let hexToBin :: Char -> Maybe String
        hexToBin char =
            case readHex [char] of
                (x, _) : _ -> Just $ printf "%04b" (x :: Int)
                _ -> Nothing
    in concat $ mapMaybe hexToBin hex


binaryToInt :: String -> Int
binaryToInt = foldl (\acc c -> 2 * acc + digitToInt c) 0


parseLiteralValue :: String -> Int -> Packet
parseLiteralValue packet version =
    let go :: [String] -> String -> Int -> Packet
        go acc [] _ = error "Malformed packet."
        go acc (h : rest) nrBlocks
            | h == '0' =
                let value = binaryToInt $ concat $ reverse $ take 4 rest : acc
                in LiteralValue version (6 + 5 * (nrBlocks + 1)) value
            | otherwise = let (group, rest') = splitAt 4 rest in go (group : acc) rest' (nrBlocks + 1)
    in go [] packet 0


parsePacket :: String -> Packet
parsePacket packet =
    let (version, packet') = splitAt 3 packet
        versionNumber = binaryToInt version
        (typeId, packet'') = splitAt 3 packet'
    in case binaryToInt typeId of
        4 -> parseLiteralValue packet'' versionNumber
        _ -> case head packet'' of
            '0' ->
                let (l, packet''') = splitAt 15 (tail packet'')
                    subpacketLength = binaryToInt l
                    go :: [Packet] -> String -> [Packet]
                    go acc [] = reverse acc
                    go acc unparsed =
                        let parsed = parsePacket unparsed
                            (_, rest) = splitAt (bitSize parsed) unparsed
                        in go (parsed : acc) rest
                    parsedSubpackets = go [] $ take subpacketLength packet'''
                    parsedLength = sum $ map bitSize parsedSubpackets
                in Operator versionNumber (6 + 15 + 1 + parsedLength) parsedSubpackets
            _ ->
                let (n, packet''') = splitAt 11 (tail packet'')
                    nrSubpackets = binaryToInt n
                    go :: [Packet] -> Int -> String -> [Packet]
                    go acc k unparsed
                        | k == nrSubpackets = reverse acc
                        | otherwise =
                            let parsed = parsePacket unparsed
                                (_, rest) = splitAt (bitSize parsed) unparsed
                            in go (parsed : acc) (k + 1) rest
                    parsedSubpackets = go [] 0 packet'''
                    parsedLength = sum $ map bitSize parsedSubpackets
                in Operator versionNumber (6 + 11 + 1 + parsedLength) parsedSubpackets


calcVersionSum :: Packet -> Int
calcVersionSum (LiteralValue v _ _) = v
calcVersionSum (Operator v _ subpackets) = v + sum (map calcVersionSum subpackets)


main :: IO()
main = do
    content <- fmap lines (readFile "./resources/input_16.txt")
    let packet = readInput $ head content
    print $ calcVersionSum $ parsePacket packet
