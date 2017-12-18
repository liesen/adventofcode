{-# LANGUAGE ViewPatterns #-}
module Main where

import Data.Foldable
import Data.Sequence (Seq, (|>), ViewL((:<)))
import qualified Data.Sequence as Seq

input = 314

-- Naive list impl
step stepSize xs x = take x (drop stepSize (cycle xs)) ++ [x]

-- Seq impl
spin xs 0 = xs
spin (Seq.viewl -> y :< ys) n = spin (ys |> y) (n - 1)

stepSeq stepSize xs x = spin xs stepSize |> x

main = do
    -- Part 1
    print $ head $ foldl' (step input) [0] [1..2017]

    -- Part 2
    -- This takes "forever" (~20 min on my machine), but the list impl
    -- won't even run
    print $ Seq.viewl $ Seq.take 1 . Seq.drop 1 . Seq.dropWhileL (/= 0) $ foldl' (stepSeq input) (Seq.singleton 0) [1..50000000]
