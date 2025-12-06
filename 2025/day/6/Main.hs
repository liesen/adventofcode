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

parseNumber = read . filter (/= ' ')

main = do
  input <- getContents

  -- Convert input into columns of strings (don't parse numbers yet)
  let lines' = lines input
      opline = last lines'
      -- Find start index of each column based on the location of operators
      (ixs', ops) = unzip [(c, op ch) | (c, ch) <- zip [0 ..] opline, ch `elem` "+*"]
      ixs = ixs' ++ [length opline + 1] -- Don't forget the last column
      -- Chop each number row into columns
      rows = map (splitLine ixs) (take (length lines' - 1) lines')
      cols = transpose rows

  -- Part 1
  let numbers = map (map parseNumber) cols
  print $ sum $ zipWith foldl1 ops numbers

  -- Part 2
  let numbers = map (map parseNumber . transpose) cols
  print $ sum $ zipWith foldl1 ops numbers
