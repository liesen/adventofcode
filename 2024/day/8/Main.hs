{-# LANGUAGE ViewPatterns #-}

import Data.Array (inRange)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set

(.-) (r, c) (dr, dc) = (r - dr, c - dc)

(.+) (r, c) (dr, dc) = (r + dr, c + dc)

anti1 bnds p1 p2 =
  filter (inRange bnds) [pmin .- d, pmax .+ d]
  where
    pmin@(rmin, cmin) = min p1 p2
    pmax@(rmax, cmax) = max p1 p2
    d = (rmax - rmin, cmax - cmin)

anti2 :: ((Int, Int), (Int, Int)) -> (Int, Int) -> (Int, Int) -> [(Int, Int)]
anti2 bnds p1 p2 =
  takeWhile (inRange bnds) (iterate (.+ d) pmax)
    ++ takeWhile (inRange bnds) (iterate (.- d) pmin)
  where
    pmin@(rmin, cmin) = min p1 p2
    pmax@(rmax, cmax) = max p1 p2
    d = (rmax - rmin, cmax - cmin)

main = do
  input <- readFile "input"
  let rows = lines input
      numrows = length rows
      numcols = length (rows !! 0)
      bnds = ((0, 0), (numrows - 1, numcols - 1))
      antennas =
        Map.fromListWith
          (<>)
          [ (a, Set.singleton (r, c))
          | (r, row) <- zip [0 ..] rows,
            (c, a) <- zip [0 ..] row,
            a /= '.'
          ]

  -- Part 1
  let antiantennas1 =
        Set.fromList
          [ a'
          | (a, Set.toList -> locs) <- Map.assocs antennas,
            x <- locs,
            y <- locs,
            x /= y,
            a' <- anti1 bnds x y
          ]
  print $ length antiantennas1

  -- Part 2
  let antiantennas2 =
        Set.fromList
          [ a'
          | (a, Set.toList -> locs) <- Map.assocs antennas,
            x <- locs,
            y <- locs,
            x /= y,
            a' <- anti2 bnds x y
          ]
  print $ length antiantennas2
