{-# LANGUAGE MultilineStrings #-}

import Data.Array
import Data.List

part1 :: Int -> [[Int]] -> Int
part1 s = snd . foldl f ([s], 0)
  where
    f (beams, numSplits) splitters =
      let (splittingBeams, passthroughBeams) = partition (`elem` splitters) beams
          splitBeams = [x + dx | x <- splittingBeams, dx <- [-1, 1]]
          beams' = nub $ sort (passthroughBeams ++ splitBeams)
       in (beams', numSplits + length splittingBeams)

part2 :: Int -> Array Int [Int] -> Integer
part2 x splitterIndices = memo ! (x, 0)
  where
    (_, ymax) = bounds splitterIndices
    xmax = foldr (max . maximum) 0 (elems splitterIndices)
    bnds = ((0, 0), (xmax + 1, ymax + 1))
    memo = array bnds [(xy, subproblem xy) | xy <- range bnds]
    subproblem (x, y)
      | y >= ymax + 1 = 1
      | x `elem` (splitterIndices ! y) = memo ! (x - 1, y + 1) + memo ! (x + 1, y + 1)
      | otherwise = memo ! (x, y + 1)

main = do
  input <- getContents
  let startLine : splitterLines = lines input
      Just s = elemIndex 'S' startLine
      splitterIndices =
        [ elemIndices '^' line
        | line <- splitterLines,
          not (all (== '.') line) -- Remove void lines
        ]

  -- Part 1
  print $ part1 s splitterIndices

  -- Part 2
  let splitterIndicesArr = listArray (0, length splitterIndices - 1) splitterIndices
  print $ part2 s splitterIndicesArr

input =
  """
  .......S.......
  ...............
  .......^.......
  ...............
  ......^.^......
  ...............
  .....^.^.^.....
  ...............
  ....^.^...^....
  ...............
  ...^.^...^.^...
  ...............
  ..^...^.....^..
  ...............
  .^.^.^.^.^...^.
  ...............
  """