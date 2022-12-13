import Control.Monad
import Data.Char
import Data.List (intercalate)
import Data.Maybe
import Data.Monoid
import Text.ParserCombinators.ReadP


data Packet =
      List [Packet]
    | Value Int
    deriving (Eq)

instance Show Packet where
    show (Value n) = show n
    show (List xs) = "[" ++ intercalate "," (map show xs) ++ "]"

parsePacket = packet
  where
    packet = List <$> between (char '[') (char ']') (sepBy (value +++ packet) (char ','))
    value = Value . read <$> munch1 isDigit

parsePacketPair :: ReadP (Packet, Packet)
parsePacketPair = (,) <$> parsePacket <*> (char '\n' *> parsePacket)

ordered :: Packet -> Packet -> Maybe Bool
ordered a b = getFirst (ordered' a b)

ordered' :: Packet -> Packet -> First Bool
ordered' (Value left) (Value right) =
    case left `compare` right of
        LT -> pure True
        GT -> pure False
        EQ -> mempty
ordered' left@(Value _) right           = ordered' (List [left]) right
ordered' left           right@(Value _) = ordered' left (List [right])
ordered' (List [])      (List [])       = mempty
ordered' (List [])      right           = pure True
ordered' left           (List [])       = pure False
ordered' (List (a:as))  (List (b:bs))   = ordered' a b <> ordered' (List as) (List bs)

main = do
    input <- readFile "input.txt"
    let [(pairs, "")] = readP_to_S ((parsePacketPair `sepBy` string "\n\n") <* skipSpaces <* eof) input

    print $ sum [i | (i, (a, b)) <- zip [1..] pairs, fromMaybe False (ordered a b)]
