module Main where

import Control.Monad (forM_)
import Data.List (group, sort)

validate1 = all ((== 1) . length) . group . sort . words

validate2 = all ((== 1) . length) . group . sort . map sort {- sort letters to find anagrams -} . words

main = do
    input <- readFile "input.txt" 
    print . length . filter validate1 . lines $ input
    print . length . filter validate2 . lines $ input
