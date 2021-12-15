{-# LANGUAGE ImportQualifiedPost, ViewPatterns #-}
import Control.Monad (guard)
import Data.Char
import Data.List
import Data.Map.Strict qualified as Map
import Data.Maybe (maybeToList)
import Data.PQueue.Prio.Min qualified as PQueue
import Data.Sequence (Seq(..))
import Data.Sequence qualified as Seq
import Data.Set (Set)
import Data.Set qualified as Set

astar :: Ord r
       => (a -> r)  -- ^ Representation
       -> (a -> [(a, Int)])  -- ^ Neighbor function (neighbor node, step cost)
       -> (a -> Int)  -- ^ Heuristic function (if const 0 then astar = bfs)
       -> [a]  -- ^ Start nodes
       -> [(a, Int)]
astar rep next heur starts = loop Set.empty q0
  where
    q0 = PQueue.fromList $ map (\start -> (heur start, (start, 0))) starts
    loop _    (PQueue.minView -> Nothing) = []
    loop seen (PQueue.minView -> Just ((x, cost), q1))
        | Set.member r seen = loop seen q1
        | otherwise = (x, cost) : loop seen1 q2
        where
          r = rep x
          seen1 = Set.insert r seen
          q2 = foldl' (\q (x', stepcost) -> let cost' = cost + stepcost
                                            in cost' `seq` PQueue.insert (cost' + heur x') (x', cost') q)
                      q1
                      (next x)

manhattan (r0, c0) (r1, c1) = abs (r1 - r0) + abs (c1 - c0)

next1 cavern p@(r, c) = do
        p' <- neighbors
        cost <- maybeToList (Map.lookup p' cavern)
        return (p', cost)
    where 
        neighbors = [(r - 1, c), (r, c + 1), (r, c - 1), (r + 1, c)]

next2 cavern f p@(r, c) = do
        p'@(r', c') <- neighbors
        guard (r' < f * numrows && c' < f * numcols)  -- Check bounds
        cost <- maybeToList (Map.lookup (r' `rem` numrows, c' `rem` numrows) cavern)
        return (p', cost <+> (r' `div` numrows) <+> (c' `div` numcols))
    where
        (maxrow, maxcol) = maximum (Map.keys cavern)
        numrows = maxrow + 1
        numcols = maxcol + 1
        neighbors = [(r - 1, c), (r, c + 1), (r, c - 1), (r + 1, c)]

-- Add risk levels
x <+> y
    | z < 10    = z
    | otherwise = z `mod` 10 + 1
    where z = x + y

main = do
    input <- readFile "input.txt"
    let cavern = Map.fromList [((r, c), digitToInt x) | (r, line) <- zip [0..] (lines input), (c, x) <- zip [0..] line]

    -- Part 1
    let goal1@(maxrow1, maxcol1) = maximum (Map.keys cavern)
        heur1 = manhattan goal1
        Just (_, ans1) = find ((== goal1) . fst) $ astar id (next1 cavern) heur1 [(0, 0)]
    print ans1

    let factor = 5
        (numrows2, numcols2) = ((maxrow1 + 1) * factor, (maxcol1 + 1) * factor)
        goal2 = (numrows2 - 1, numcols2 - 1)
        heur2 = manhattan goal2
        Just (_, ans2) = find ((== goal2) . fst) $ astar id (next2 cavern factor) heur2 [(0, 0)]
    print ans2
