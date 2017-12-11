module Main where

import Data.List
import Data.Char

parse s = read ("[" ++ s ++ "]") :: [Int]

step (skipSize, xs) i =
    let (ys, zs) = splitAt i xs
        xs' = reverse ys ++ zs
        j = (i + skipSize) `mod` length xs
        (ys', zs') = splitAt j xs'
    in (skipSize + 1, zs' ++ ys')

test = scanl step (0, [0..4]) [3,4,1,5]

main = do
    input <- parse `fmap` readFile "input.txt"
    print $ foldl step (0, [0..255]) input
