{-# LANGUAGE ViewPatterns #-}
import Control.Monad
import Data.Array
import Data.Char
import Data.List
import Data.Ord
import qualified Data.IntSet as IntSet
import Data.IntSet (IntSet)
import qualified Data.Set as Set
import Data.Set (Set)
import qualified Data.Sequence as Seq
import Data.Sequence (Seq (..))

parse s = listArray ((0, 0), (numrows - 1, numcols - 1)) (concat rows)
  where
    rows = lines s
    numrows = length rows
    numcols = length (head rows)

data State = State
    { pos :: (Int, Int)
    , keys :: IntSet
    , grid :: Array (Int, Int) Char
    } deriving (Show)

bfs :: Ord r => (a -> r) -> (a -> [(a, Int)]) -> a -> [(a, Int)]
bfs rep next start = loop Set.empty (Seq.fromList [(start, 0)])
  where
    loop _    Empty = []
    loop seen ((x, cost) :<| q1)
        | Set.member r seen = loop seen q1
        | otherwise = (x, cost) : loop seen1 q2
        where
          r = rep x
          seen1 = Set.insert r seen
          q2 = q1 <> Seq.fromList (map (\(x', stepcost) -> (x', cost + stepcost)) (next x))

rep (State pos keys grid) = (pos, keys)

next (State pos@(r, c) keys grid) = do
    (dr, dc) <- [(-1, 0), (1, 0), (0, -1), (0, 1)]
    let pos'@(r', c') = (r + dr, c + dc)
    guard (inRange (bounds grid) pos')
    let z = grid ! pos'
    guard (z /= '#')

    if isDigit z && digitToInt z `IntSet.notMember` keys
        then return (State pos' (IntSet.insert (digitToInt z) keys) grid, 1)
        else return (State pos' keys grid, 1)

main = do
    input <- readFile "input.txt"
    let grid = parse input
        digits = map digitToInt (filter isDigit (elems grid))
        [start] = [pos | (pos, x) <- assocs grid, x == '0']
        allKeys = IntSet.fromList digits
        state0 = State start mempty grid

    -- Part 1
    let done1 (State _ keys _) = keys == IntSet.fromList (digits \\ [0])
        paths = filter (done1 . fst) $ bfs rep next state0
        (state1, ans1) = head paths
    print ans1

    -- Part 2
    let ans2 = minimum $ map (\(s, n) -> let Just (_, m) = find ((== start) . pos . fst) (bfs rep next s) in n + m) paths
    print ans2
