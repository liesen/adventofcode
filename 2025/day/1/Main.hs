{-# LANGUAGE MultilineStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

input =
  """
  L68
  L30
  R48
  L5
  R60
  L55
  L1
  L99
  R14
  L82
  """

data Dial = Dial {value :: Int, part1 :: Int, part2 :: Int} deriving (Show)

rotate (Dial d z x) ('L' : (read -> n)) =
  let (dx', d') = (d - n) `divMod` 100
      dz = if d' == 0 then 1 else 0
      dx = -dx' + dz - if d == 0 then 1 else 0
   in Dial d' (z + dz) (x + dx)
rotate (Dial d z x) ('R' : (read -> n)) =
  let (dx, d') = (d + n) `divMod` 100
      dz = if d' == 0 then 1 else 0
   in Dial d' (z + dz) (x + dx)

main = do
  input <- getContents
  let Dial {..} = foldl rotate (Dial 50 0 0) (lines input)

  print part1
  print part2
