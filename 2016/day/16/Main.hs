{-# LANGUAGE OverloadedStrings #-}
import qualified Data.ByteString.Char8 as C
import Data.Monoid
import Data.Bits
import Data.List

--- Day 16: Dragon Checksum ---
flip' '1' = '0'
flip' '0' = '1'

checksum x
    | C.null x  = C.empty
    | otherwise = C.cons (if C.index x 0 == C.index x 1 then '1' else '0') (checksum (C.drop 2 x))

dragon n = fold . unfold
  where
    unfold = until ((>= n) . C.length) go
      where go a = let b = C.map flip' (C.reverse a) in a <> "0" <> b

    fold = until (odd . C.length) checksum . C.take n

main1 = print $ dragon 272 (C.pack "10010000000110000")

--- Part Two ---
data Dragon a = String Int a
              -- | Flip Int (Dragon a)
              -- | Reverse Int (Dragon a)
              | FlipReverse Int (Dragon a)
              | Concat Int (Dragon a) (Dragon a)
  deriving (Eq, Show)

dragonLength (String n _) = n
-- dragonLength (Flip n _)     = n
-- dragonLength (Reverse n _)  = n
dragonLength (FlipReverse n _) = n
dragonLength (Concat n _ _) = n

dragon2 n a = unfold
  where
    unfold = until ((>= n) . dragonLength) go a 
      where go a = let m = dragonLength a in Concat (2 * m + 1) a (FlipReverse m a)

flatten (String m s)      = s
-- flatten (Flip n (Reverse m s)) = map flip' (reverse (flatten s))
flatten (FlipReverse m s) = C.map flip' (C.reverse (flatten s))
flatten (Concat n x y)    = flatten x <> "0" <> flatten y

asdf2 = dragon2 272 (String (C.length a) a) -- (readBits a))
  where a = C.pack "10010000000110000"

-- main2 = print $ dragon 35651584 "10010000000110000"
-- main2 = print $ dragon2 272 (readBits "10010000000110000")

readBits :: String -> Integer
readBits = foldl' f 0
  where
    f a '1' = a `shiftL` 1 .|. 1
    f a '0' = a `shiftL` 1

-- main = main2

