{-# LANGUAGE LambdaCase #-}
module Main where

import Data.Array
import Data.Bits
import Data.Char
import Data.List
import Data.Set (Set)
import qualified Data.Set as Set
import Text.Printf

input = "oundnydw"

newtype KnotHash = KnotHash (Array Int Int)

instance Show KnotHash where
    show (KnotHash denseHash) = concatMap (printf "%02x") denseHash

knotHash :: String -> KnotHash
knotHash s =
    let input = concat $ replicate 64 $ map ord s ++ magic
        (_pos, _skipSize, a) = foldl step (0, 0, listArray (0, 255) [0..255]) input
        sparseHash = elems a
        denseHash = listArray (0, 15) $ map (foldl1 xor) (chunksOf 16 sparseHash)
    in KnotHash denseHash
  where
    magic = [17, 31, 73, 47, 23]
    step (pos, skipSize, xs) len =
        let is = map (`mod` 256) [pos..pos + len - 1]
            as = map (xs !) (reverse is)
        in ((pos + len + skipSize) `mod` 256, skipSize + 1, xs // (is `zip` as))
    chunksOf n = unfoldr $ \case
                   [] -> Nothing
                   xs -> Just (splitAt n xs)

bits :: KnotHash -> String
bits = concatMap (printf "%04b") . map hexDigitToInt . show

bits' = map f . bits
  where f '0' = '.'
        f '1' = '#'

hexDigitToInt c
    | c >= '0' && c <= '9' = ord c - ord '0'
    | c >= 'a' && c <= 'f' = ord c - (ord 'a' - 10)
    | otherwise            = ord c - (ord 'A' - 10)

type Disk = Array (Int, Int) Bool

disk :: String -> Disk
disk input = listArray ((0, 0), (127, 127)) $ concatMap row $ map (\n -> knotHash (input ++ "-" ++ show n)) [0..127]
  where row = map (toEnum . digitToInt) . bits

flood :: Disk -> (Int, Int) -> Set (Int, Int)
flood disk p = go p Set.empty
  where
    go p@(x, y) s
        | not (inRange (bounds disk) p) = s
        | not (disk ! p) = s
        | Set.member p s = s
        | otherwise      =
            go (x + 1, y) $
            go (x - 1, y) $
            go (x, y + 1) $
            go (x, y - 1) $
            Set.insert p s

regions disk = let (n, _, _) = until p f (0, Set.fromList (range (bounds disk)), Set.empty)
               in n
  where
    p (n, unseen, seen) = Set.null unseen
    f (n, unseen, seen) =
        let Just (p, unseen') = Set.minView unseen
            s = flood disk p
        in (n + if Set.null s then 0 else 1,
            Set.difference unseen' s,
            Set.insert p (Set.union seen s))

main = do
    let d = disk input

    -- Part 1
    print $ sum . map fromEnum $ elems d

    -- Part 2
    print $ regions d
    
