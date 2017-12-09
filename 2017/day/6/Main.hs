module Main where

import Control.Monad

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
         in accumArray (+) 0 bnds $ (i, -x) : assocs a ++ zip (cycle $ map (\j -> (j + i + 1) `mod` (n + 1)) $ range bnds) (replicate x 1)

run a = until p f (Map.empty, a, 0)
  where p (states, state, i) = Map.member state states  -- state has been seen before
        f (states, state, i) = (Map.insert state i states, step state, i + 1)

main = do
    input <- readFile "input.txt"
    let (states, state, i) = run (parse input)
    print i
    let Just j = Map.lookup state states
    print $ i - j
