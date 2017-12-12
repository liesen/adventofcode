{-# LANGUAGE LambdaCase #-}
module Main where

import Data.Array
import Data.Bits
import Data.Char
import Data.List
import Text.Printf

parse s = read ("[" ++ s ++ "]") :: [Int]

step :: (Int, Int, Array Int Int) -> Int -> (Int, Int, Array Int Int)
step (pos, skipSize, xs) len =
    let is = map (`mod` (n + 1)) [pos..pos + len - 1]
        as = map (xs !) (reverse is)
    in ((pos + len + skipSize) `mod` length xs, skipSize + 1, xs // (is `zip` as))
  where
    (0, n) = bounds xs

denseHash sparseHash = map (foldl1 xor) (chunksOf 16 sparseHash)
  where
    chunksOf n = unfoldr $ \case
                   [] -> Nothing
                   xs -> Just (splitAt n xs)

main = do
    input <- readFile "input.txt"

    -- Part 1
    let (pos, skipSize, a) = foldl step (0, 0, listArray (0, 255) [0..255]) $ parse input
    print ((a ! 0) * (a ! 1))

    -- Part 2
    let input' = head (lines input)  -- Drop trailing whitespace
        (pos, skipSize, sparseHash) = foldl step (0, 0, listArray (0, 255) [0..255]) $ concat . replicate 64 $ (map ord input' ++ [17, 31, 73, 47, 23])
        showHex = concatMap (printf "%02x")
        digest = showHex $ denseHash (elems sparseHash)
    putStrLn digest

