{-# LANGUAGE MultilineStrings #-}
{-# LANGUAGE ViewPatterns #-}

import Data.Either
import Data.List (sort)

input =
  """
  3-5
  10-14
  16-20
  12-18

  1
  5
  8
  11
  17
  32
  """

type Range = (Int, Int)

parse :: String -> ([Range], [Int])
parse = partitionEithers . go . lines
  where
    go [] = []
    -- Separator -> skip
    go ("" : rest) = go rest
    -- Range
    go ((break (== '-') -> (read -> start, '-' : (read -> end))) : rest) = Left (start, end) : go rest
    -- Incredient ID
    go ((read -> ingredientId) : rest) = Right ingredientId : go rest

fresh :: [Range] -> Int -> Bool
fresh ranges i = any (\(start, end) -> i >= start && i <= end) ranges

count :: [Range] -> Int
count [] = 0
count [(x0, x1)] = x1 - x0 + 1
count ((x0, x1) : (y0, y1) : zs)
  | x1 >= y0 = count ((x0, max x1 y1) : zs) -- Ranges overlap -> merge
  | otherwise = (x1 - x0 + 1) + count ((y0, y1) : zs) -- Ranges disjoint

main = do
  input <- getContents
  let (ranges, ingredientIds) = parse input

  -- Part 1
  print $ length $ filter (fresh ranges) ingredientIds

  -- Part 2
  print $ count $ sort ranges
