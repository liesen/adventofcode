{-# LANGUAGE RecordWildCards #-}

import Data.List
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Ord
import Data.Set (Set)
import Data.Set qualified as Set

data Platform = Platform
  { bounds :: (Int, Int),
    movable :: Set (Int, Int),
    fixed :: Set (Int, Int)
  }
  deriving (Eq)

instance Show Platform where
  show (Platform (maxrow, maxcol) movable fixed) =
    unlines
      [ [char (r, c) | c <- [0 .. maxcol]]
        | r <- [0 .. maxrow]
      ]
    where
      char p
        | p `elem` movable = 'O'
        | p `elem` fixed = '#'
        | otherwise = '.'

tilt :: (Int, Int) -> Platform -> Platform
tilt (dr, dc) (Platform bounds@(maxrow, maxcol) movable fixed) =
  Platform bounds movable' fixed
  where
    sortByTilt = sortBy (comparing (\(r, c) -> (r * (-dr), c * (-dc))))
    movable' = fst $ until (not . snd) slide (movable, True)
    slide = foldl slide1 (mempty, False) . sortByTilt . Set.toList . fst
    slide1 (rocks', moved) (r, c)
      | r' >= 0
          && r' <= maxrow
          && c' >= 0
          && c' <= maxcol
          && (r', c') `notElem` fixed
          && (r', c') `notElem` rocks' =
          (Set.insert (r', c') rocks', True)
      | otherwise = (Set.insert (r, c) rocks', moved)
      where
        r' = r + dr
        c' = c + dc

load (Platform (maxrow, maxcol) movable fixed) =
  sum [maxrow - r + 1 | (r, c) <- Set.toList movable]

spinCycle = tilt (0, 1) . tilt (1, 0) . tilt (0, -1) . tilt (-1, 0)

main = do
  input <- readFile "input"
  let tiles = [((r, c), ch) | (r, row) <- zip [0 ..] (lines input), (c, ch) <- zip [0 ..] row]
      maxrow = maximum [r | ((r, c), _) <- tiles]
      maxcol = maximum [c | ((r, c), _) <- tiles]
      bounds = (maxrow, maxcol)
      movable = Set.fromList [p | (p, ch) <- tiles, ch == 'O']
      fixed = Set.fromList [p | (p, ch) <- tiles, ch == '#']
      platform = Platform {..}

  -- Part 1
  print $ load $ tilt (-1, 0) platform

  -- Part 2
  let bigT = 1_000_000_000
      rep Platform {..} = movable
      -- Find cycle start, end and length
      (memo, platform1, t1) =
        until
          (\(memo, platform, t) -> Map.member (rep platform) memo)
          (\(memo, platform, t) -> (Map.insert (rep platform) t memo, spinCycle platform, t + 1))
          (mempty, platform, 0)
  let Just t0 = Map.lookup (rep platform1) memo
      n = t1 - t0
      (d, m) = (bigT - t1) `divMod` n
  print $ load $ iterate spinCycle platform1 !! m
