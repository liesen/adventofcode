module Main where

import Control.Monad

import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map

import Data.Array
import Data.List
import Data.Ord

type Memory = Array Int Int

testInput :: Memory
testInput = listArray (0, 3) [0,2,7,0]

parse :: String -> Memory
parse s = let xs = map read (words s)
          in listArray (0, length xs - 1) xs

maxi :: Memory -> (Int, Int)
-- maximumBy (comparing snd) won't work because it will yield
-- the /last/ index where a maximal element is found
maxi = minimumBy (comparing (negate . snd)) . assocs

step a = let bnds@(0, n) = bounds a
             (i, x) = maxi a
         in accumArray (+) 0 (0, n) $ (i, -x) : assocs a ++ zip (cycle $ map (\j -> (j + i + 1) `mod` (n + 1)) $ range bnds) (replicate x 1)

run step a = until (\(as, a, i) -> Map.member a as) (\(as, a, i) -> (Map.insert a i as, step a, i + 1)) (Map.empty, a, 0)

main = do
    input <- readFile "input.txt"
    let (as, a, i) = run step (parse input)
    print i
    let Just j = Map.lookup a as
    print $ i - j
