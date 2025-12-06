{-# LANGUAGE MultilineStrings #-}

import Data.List (transpose)

input =
  """
  123 328  51 64 
   45 64  387 23 
    6 98  215 314
  *   +   *   +  
  """

op '+' = (+)
op '*' = (*)

splitLine xs ln = zipWith split xs (drop 1 xs)
  where
    split i j = take (j - i - 1) (drop i ln)

main = do
  input <- getContents
  let lines' = lines input
      opline = last lines'
      -- Column start indices
      (ixs', ops) = unzip [(c, op ch) | (c, ch) <- zip [0 ..] opline, ch `elem` "+*"]
      ixs = ixs' ++ [length opline + 1]
      rows = map (splitLine ixs) (take (length lines' - 1) lines')
      cols = transpose rows

  -- Part 1
  let numbers = map (map (read . filter (/= ' '))) cols
  print $ sum $ zipWith foldl1 ops numbers

  -- Part 2
  let numbers = map (map (read . filter (/= ' ')) . transpose) cols
  print $ sum $ zipWith foldl1 ops numbers
