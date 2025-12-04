{-# LANGUAGE MultilineStrings #-}

import Data.Set (Set)
import Data.Set qualified as Set

input =
  """
  ..@@.@@@@.
  @@@.@.@.@@
  @@@@@.@.@@
  @.@@@@..@.
  @@.@@@@.@@
  .@@@@@@@.@
  .@.@.@.@@@
  @.@@@.@@@@
  .@@@@@@@@.
  @.@.@@@.@.
  """

neighbors (r, c) = [(r + dr, c + dc) | dr <- [-1 .. 1], dc <- [-1 .. 1], not (dr == 0 && dc == 0)]

accessible rolls = Set.filter p rolls
  where
    p v = length (filter (`elem` rolls) (neighbors v)) < 4

main = do
  input <- getContents
  let rolls =
        Set.fromList
          [ (r, c)
          | (r, row) <- zip [0 ..] (lines input),
            (c, '@') <- zip [0 ..] row
          ]

  -- Part 1
  print $ Set.size $ accessible rolls

  -- Part 2
  let remaining = until (Set.null . accessible) (\rolls -> rolls `Set.difference` accessible rolls) rolls
  print $ Set.size rolls - Set.size remaining
