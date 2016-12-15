import Control.Monad
import Data.Bits (popCount)

import Data.Set (Set)
import qualified Data.Set as Set

--- Day 13: A Maze of Twisty Little Cubicles ---

-- Check if coordinate is an open space or a wall
space n (x, y) =
    let z = x * x + 3 * x + 2 * x * y + y + y * y
        z' = z + n
    in popCount z' `mod` 2 == 0
        
type Pos = (Int, Int)

-- BFS with keeping track on visited coordinates: first solution is
-- guaranteed to be the shortest
--
-- https://wiki.haskell.org/Haskell_Quiz/Numeric_Maze/Solution_Dolio
search1 :: Int -> Pos -> (Pos, Int)
search1 n q = search' [((1, 1), 0)] Set.empty
  where
    search' :: [(Pos, Int)] -> Set Pos -> (Pos, Int)
    search' ps seen
        | (p:_) <- filter ((== q) . fst) ps = p
        | otherwise                         = search' ps' seen'
      where
        seen' = seen `Set.union` Set.fromList (map fst ps')
        ps' = do ((x, y), l) <- ps
                 (dx, dy) <- [(0, 1), (1, 0), (0, -1), (-1, 0)]
                 let p@(x', y') = (x + dx, y + dy)
                 guard $ x' >= 0 && y' >= 0
                 guard $ space n p
                 guard $ p `Set.notMember` seen
                 return (p, l + 1)
                 
test = search1 10 (7, 4)

main1 = print . snd $ search1 1364 (31, 39)

--- Part Two ---

search2 n q = search' [((1, 1), 0)] Set.empty
  where
    search' ps seen
        | (p:_) <- filter ((== q) . snd) ps = Set.size seen
        | otherwise                         = search' ps' seen'
      where
        seen' = seen `Set.union` Set.fromList (map fst ps')

        ps' :: [(Pos, Int)]
        ps' = do ((x, y), l) <- ps
                 (dx, dy) <- [(0, 1), (1, 0), (0, -1), (-1, 0)]
                 let p@(x', y') = (x + dx, y + dy)
                 guard $ x' >= 0 && y' >= 0  -- negative values are invalid
                 guard $ space n p
                 guard $ p `Set.notMember` seen
                 return (p, l + 1)

main2 = print $ search2 1364 50

main = main1 >> main2
