{-# LANGUAGE ViewPatterns #-}
import Control.Arrow
import Control.Monad
import Data.Array
import Data.List
import Data.Ord
import Data.Function (on)
import qualified Data.PQueue.Prio.Min as PQueue
import qualified Data.Set as Set
import Data.Set (Set)
import qualified Data.Sequence as Seq
import Data.Sequence (Seq (..))
import Debug.Trace

-- Full node
data Node = Node
    { loc :: (Int, Int)
    , size :: Int
    , used :: Int
    , avail :: Int
    , use :: Int
    } deriving (Eq, Ord, Show)

parse (words -> [drop (length "/dev/grid/node-") -> (break (== '-') -> ('x':(read -> x), '-':'y':(read -> y))),
                 break (== 'T') -> (read -> size, "T"),
                 break (== 'T') -> (read -> used, "T"),
                 break (== 'T') -> (read -> avail, "T"),
                 break (== '%') -> (read -> use, "%")]) =
    Node (x, y) size used avail use

viablePairs nodes = [(a, b) | a <- nodes, b <- nodes, used a > 0, loc a /= loc b, used a <= avail b]

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

-- Simple node: only contains Size and Used
newtype Use = Use (Int, Int)
    deriving (Eq, Ord)

instance Show Use where
    show (Use (used, size)) = show used ++ "T/" ++ show size ++ "T"

-- Search state
data State = State (Int, Int) (Array (Int, Int) Use)
    deriving (Show, Eq, Ord)

newState nodes = State (x0, 0) arr
  where
    arr = array (minimum locs, maximum locs) (map (loc &&& use) nodes)
    locs = map loc nodes
    x0 = maximum [x | (x, y) <- locs, y == 0]
    use node = Use (used node, size node)

bfs :: Ord r => (a -> r) -> (a -> [(a, Int)]) -> a -> [(a, Int)]
bfs rep next start = loop Set.empty (Seq.fromList [(start, 0)])
  where
    loop _    Empty = []
    loop seen ((x, cost) :<| q1)
        | Set.member r seen = loop seen q1
        | otherwise = traceShow (Set.size seen1) $ (x, cost) : loop seen1 q2
        where
          r = rep x
          seen1 = Set.insert r seen
          q2 = q1 <> Seq.fromList (map (\(x', stepcost) -> (x', cost + stepcost)) (next x))

astar :: Ord r => (a -> r) -> (a -> [(a, Int)]) -> (a -> Int) -> a -> [(a, Int)]
astar rep next heuristic start = loop Set.empty (PQueue.singleton 0 (start, 0))
  where
    loop _    (PQueue.minView -> Nothing) = []
    loop seen (PQueue.minView -> Just ((x, cost), q1))
        | Set.member r seen = loop seen q1
        | Set.size seen1 `mod` 1000 == 0 = traceShow (Set.size seen1) $ (x, cost) : loop seen1 q2
        | otherwise = (x, cost) : loop seen1 q2
        where
          r = rep x
          seen1 = Set.insert r seen
          q2 = foldl' (\q (x', stepcost) -> let cost' = cost + stepcost in cost' `seq` PQueue.insert (cost' + heuristic x') (x', cost') q) q1 (next x)

-- Use the manhattan distance as heuristic
heuristic (State (x, y) _) = x + y

next :: State -> [(State, Int)]
next (State pos arr) =
    [ (State pos' arr', 1)
    | (pos1@(x1, y1), Use (used1, size1)) <- assocs arr
    , used1 /= 0 -- Don't move empty nodes!
    , (dx, dy) <- [(0, -1), (0, 1), (-1, 0), (1, 0)]
    , let pos2@(x2, y2) = (x1 + dx, y1 + dy)
    , inRange (bounds arr) pos2
    , pos1 /= pos2
    , let Use (used2, size2) = arr ! pos2
    , used1 + used2 <= size2
    , let pos' = if pos1 == pos then pos2 else pos
    , let arr' = arr // [(pos1, Use (0, size1)), (pos2, Use (used1 + used2, size2))]
    ]

done (State (0, 0) _) = True
done _                = False

main = do
    input <- readFile "input.txt"
    let nodes = map parse . drop 2 . lines $ input

    -- Part 1
    print $ length $ viablePairs nodes

    -- Part 2
    print $ snd $ head $ filter (done . fst) $ astar id next heuristic (newState nodes)
