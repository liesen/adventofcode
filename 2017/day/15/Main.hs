{-# LANGUAGE ViewPatterns #-}
module Main where

import Data.Bits

parseLine (words -> ["Generator", gen, "starts","with",(read -> seed)]) = (gen, seed)

parse = map parseLine . lines

generator factor = tail . iterate (\prev -> (prev * factor) `mod` 2147483647)

judgeCmp :: Integer -> Integer -> Bool
judgeCmp a b = a .&. mask == b .&. mask
  where
    mask = foldl setBit 0 [0..15]

judge as bs = zipWith judgeCmp as bs

main = do
    input <- parse <$> readFile "input.txt"

    let Just (generator 16807 -> a) = lookup "A" input
        Just (generator 48271 -> b) = lookup "B" input

    -- Part 1
    print $ sum $ map fromEnum $ take 40000000 $ judge a b

    -- Part 2
    let a4 = filter (\n -> n `mod` 4 == 0) a
        b8 = filter (\n -> n `mod` 8 == 0) b
    print $ sum $ map fromEnum $ take 5000000 $ judge a4 b8
