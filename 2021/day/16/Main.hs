{-# LANGUAGE ViewPatterns #-}
import Data.Char
import Text.ParserCombinators.ReadP

data Packet a = Packet a a (Contents a)
    deriving (Show)

data Contents a
    = Literal a
    | Operator a [Packet a]
    deriving (Show)

instance Functor Packet where
    fmap f (Packet ver tid a) = Packet (f ver) (f tid) (fmap f a)

instance Functor Contents where
    fmap f (Literal a) = Literal (f a)
    fmap f (Operator a as) =  Operator (f a) (map (fmap f) as)

-- Plan is to first convert input into binary text then
-- use parser combinators to parse it
hex2bin '0' = "0000"
hex2bin '1' = "0001"
hex2bin '2' = "0010"
hex2bin '3' = "0011"
hex2bin '4' = "0100"
hex2bin '5' = "0101"
hex2bin '6' = "0110"
hex2bin '7' = "0111"
hex2bin '8' = "1000"
hex2bin '9' = "1001"
hex2bin 'A' = "1010"
hex2bin 'B' = "1011"
hex2bin 'C' = "1100"
hex2bin 'D' = "1101"
hex2bin 'E' = "1110"
hex2bin 'F' = "1111"

bin2dec = foldr f 0 . reverse
    where
        f x y = digitToInt x + y * 2

parseBit = satisfy (`elem` "01")

parseVersion = count 3 parseBit 

parseTypeId = count 3 parseBit

parsePacket = do
    v <- parseVersion
    t <- parseTypeId
    c <- parseContents t
    return $ Packet v t c

parseContents "100" = parseLiteral
parseContents _     = parseOperator

parseLiteral = Literal <$> literal
    where
        literal = do
            x:xs <- count 5 parseBit

            if x == '0'
                then pure xs
                else (++) <$> pure xs <*> literal

parseOperator = do
    lengthTypeId <- parseBit

    case lengthTypeId of
        -- Parse packets from number of bits
        '0' -> do
           len <- count 15 parseBit
           content <- count (bin2dec len) parseBit
           case readP_to_S (many parsePacket <* eof) content of
               [(subpackets, "")] -> return $ Operator len subpackets
        -- Parse a number of packets
        '1' -> do
           num <- count 11 parseBit
           subpackets <- count (bin2dec num) parsePacket
           return (Operator num subpackets)

-- Get all versions inside the package
versions :: Num a => Packet a -> [a]
versions (Packet v _ a) = v : versions' a
    where
        versions' (Literal a) = []
        versions' (Operator _ as) = concatMap versions as

-- Calculate the value of the expression that the packet represents
eval :: (Ord a, Num a) => Packet a -> a
eval (Packet _ 0 a) = sum (evalContents a)
eval (Packet _ 1 a) = product (evalContents a)
eval (Packet _ 2 a) = minimum (evalContents a)
eval (Packet _ 3 a) = maximum (evalContents a)
eval (Packet _ 4 (Literal v)) = v
eval (Packet _ 5 (evalContents -> [x, y]))
    | x > y     = 1
    | otherwise = 0
eval (Packet _ 6 (evalContents -> [x, y]))
    | x < y     = 1
    | otherwise = 0
eval (Packet _ 7 (evalContents -> [x, y]))
    | x == y    = 1
    | otherwise = 0

evalContents :: (Ord a, Num a) => Contents a -> [a]
evalContents (Literal a) = [a]
evalContents (Operator _ as) = map eval as

main = do
     input <- readFile "input.txt"
     let [(packet, _)] = readP_to_S parsePacket (concatMap hex2bin input)

     -- Part 1
     print $ sum $ versions $ fmap bin2dec packet

     -- Part 2
     print $ eval $ fmap bin2dec packet
