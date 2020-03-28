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

    if isDigit z
        then return (State pos' (IntSet.insert (digitToInt z) keys) grid, 1)
        else return (State pos' keys grid, 1)

main = do
    input <- readFile "input.txt"
    let grid = parse input
        digits = map digitToInt (filter isDigit (elems grid))
        [start] = [pos | (pos, x) <- assocs grid, x == '0']
        allKeys = IntSet.fromList digits
        state0 = State start (IntSet.singleton 0) grid
        done (State _ keys _) = keys == allKeys

    -- Part 1
    let (_, ans) = head $ filter (done . fst) $ bfs rep next state0
    print ans
