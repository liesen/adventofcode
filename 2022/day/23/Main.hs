{-# LANGUAGE ImportQualifiedPost #-}
import Control.Monad
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Foldable
import Data.List
import Data.Maybe
import Data.Map (Map)
import Data.Map.Strict qualified as Map

import Debug.Trace

-- Directions
[nw, n, ne,
  w,    e,
 sw, s, se] = [(dy, dx) | dy <- [-1, 0, 1], dx <- [-1, 0, 1], dy /= 0 || dx /= 0]

directions = [
        [n, ne, nw],
        [s, se, sw],
        [w, nw, sw],
        [e, ne, se]
    ]

-- Translate coordinate
(y, x) .+ (dy, dx) = (y + dy, x + dx)

run elves = unfoldr step (elves, directions)

step (elves, dirs)
    | all (uncurry (==)) uniquePropositions = Nothing
    | otherwise = Just (elves, (Set.map adjust elves, tail dirs ++ [head dirs]))
  where
    adjust p = fromMaybe p (lookup p uniquePropositions)

    uniquePropositions = filter unique propositions
      where
        unique (p, p') = all (\(q, q') -> q == p || q' /= p') propositions

    propositions = map (\p -> (p, proposePos elves dirs p)) (Set.toList elves)

proposePos elves dirs pos
    | all ((`notElem` elves) . (.+ pos)) [nw, n, ne, w, e, sw, s, se] = pos
    | Just (dir:_) <- find (all ((`notElem` elves) . (.+ pos))) dirs = pos .+ dir
    | otherwise = pos

score elves = tileCount - length elves
  where
    ymin = minimum (Set.map fst elves)
    ymax = maximum (Set.map fst elves)
    xmin = minimum (Set.map snd elves)
    xmax = maximum (Set.map snd elves)
    tileCount = (ymax - ymin + 1) * (xmax - xmin + 1)

main = do
    input <- readFile "input.txt"
    let elves = Set.fromList [(y, x) | (y, ln) <- zip [0..] (lines input), (x, ch) <- zip [0..] ln, ch == '#']

    -- Part 1
    print $ score $ run elves !! 10

    -- Part 2
    print $ (+ 1) $ length $ run elves  -- This takes "forever"
