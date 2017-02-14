{-# LANGUAGE ViewPatterns #-}
import Control.Arrow
import Control.Monad
import Data.Array
import Data.List
import Data.Ord
import Data.Function (on)

data Node = Node { loc :: (Int, Int), size :: Int, used :: Int, avail :: Int, use :: Int } deriving (Eq, Ord, Show)

parse (words -> [drop (length "/dev/grid/node-") -> (break (== '-') -> ('x':(read -> x), '-':'y':(read -> y))),
                 break (== 'T') -> (read -> size, "T"),
                 break (== 'T') -> (read -> used, "T"),
                 break (== 'T') -> (read -> avail, "T"),
                 break (== '%') -> (read -> use, "%")]) =
    Node (x, y) size used avail use

viablePairs nodes = [(a, b) | a <- nodes, b <- nodes, used a > 0, loc a /= loc b, used a <= avail b]

main1 = do
    input <- readFile "input.txt"
    let nodes = map parse . drop 2 . lines $ input
    print (length (viablePairs nodes))

--- Part Two ---
test2 = [
    "Filesystem            Size  Used  Avail  Use%",
    "/dev/grid/node-x0-y0   10T    8T     2T   80%",
    "/dev/grid/node-x0-y1   11T    6T     5T   54%",
    "/dev/grid/node-x0-y2   32T   28T     4T   87%",
    "/dev/grid/node-x1-y0    9T    7T     2T   77%",
    "/dev/grid/node-x1-y1    8T    0T     8T    0%",
    "/dev/grid/node-x1-y2   11T    7T     4T   63%",
    "/dev/grid/node-x2-y0   10T    6T     4T   60%",
    "/dev/grid/node-x2-y1    9T    8T     1T   88%",
    "/dev/grid/node-x2-y2    9T    6T     3T   66%"
  ]

{-
trans (pos@(x, y), grid) = do
    (dx, dy) <- [(0, -1), (0, 1), (-1, 0), (1, 0)] 
    let pos'@(x', y') = (x + dx, y + dy)
    guard $ x' >= 0 && x' <= 3 && y' >= 0 && y' <= 3
    let src = grid ! pos
        dst = grid ! pos'
    guard $ 
    let grid' = 
-}

gridArray nodes = array (minimum locs, maximum locs) (map (loc &&& id) nodes)
  where
    locs = map loc nodes

viableNeighbors grid = do
    (pos@(x, y), node) <- assocs grid
    (dx, dy) <- [(0, -1), (0, 1), (-1, 0), (1, 0)] 
    let pos'@(x', y') = (x + dx, y + dy)
    guard $ inRange (bounds grid) pos'
    let node' = grid ! pos'
    guard $ used node <= avail node'
    return (pos, pos')



-- [(a, b) | a <- nodes, b <- nodes, used a > 0, loc a /= loc b, used a <= avail b]

main = main1
