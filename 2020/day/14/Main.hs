import Text.ParserCombinators.ReadP
import Data.Char
import Data.Bits
import qualified Data.IntMap as IntMap

data Stmt
    = Mask String  -- mask = value
    | Mem Int Int  -- mem[key] = value
  deriving (Eq, Show)

numBits = 36

stmt = mask +++ mem

parse = stmt `endBy` char '\n' <* eof

mask :: ReadP Stmt
mask = Mask <$> (string "mask = " *> count numBits (satisfy (`elem` "01X")))

mem :: ReadP Stmt
mem = Mem <$> (string "mem" *> between (char '[') (char ']') number) <*> (string " = " *> number)

number = read <$> munch1 isDigit

eval1 (_, mem) (Mask newMask) = (Just newMask, mem)
eval1 (Just mask, mem) (Mem key value) = (Just mask, IntMap.insert key (applyMask value mask) mem)

applyMask :: Int -> String -> Int
applyMask value = snd . foldr f (0, 0)
  where
    f '0' (i, v) = (i + 1, v `clearBit` i)
    f '1' (i, v) = (i + 1, v `setBit` i)
    f 'X' (i, v)
        | value `testBit` i = (i + 1, v `setBit` i)
        | otherwise         = (i + 1, v `clearBit` i)

eval2 (_, mem) (Mask newMask)  = (Just newMask, mem)
eval2 (Just mask, mem) (Mem key value) =
    (Just mask, mem <> IntMap.fromList [(key', value) | key' <- expandMask value mask])

expandMask :: Int -> String -> [Int]
expandMask value = snd . foldr f (0, [value])
  where
    f '0' (i, vs) = (i + 1, vs)
    f '1' (i, vs) = (i + 1, map (`setBit` i) vs)
    f 'X' (i, vs) = (i + 1, map (`clearBit` i) vs <> map (`setBit` i) vs)

run step = sum . snd . foldl step (Nothing, mempty)

main = do
    input <- readFile "input.txt"
    let [(prog, "")] = readP_to_S parse input

    -- Part 1
    print $ run eval1 prog
    
    -- Part 2
    print $ run eval2 prog
