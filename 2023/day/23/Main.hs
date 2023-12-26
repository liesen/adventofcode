{-# LANGUAGE ViewPatterns #-}

import Control.Arrow
import Data.List
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe
import Data.Set (Set)
import Data.Set qualified as Set

parse :: String -> (((Int, Int), (Int, Int)), Map (Int, Int) Char)
parse s = (bounds, grid)
  where
    rows = filter (/= "") (lines s)
    numrows = length rows
    numcols = length (head rows)
    bounds = ((0, 0), (numrows - 1, numcols - 1))
    grid = Map.fromList [((r, c), ch) | (r, cs) <- zip [0 ..] rows, (c, ch) <- zip [0 ..] cs]

type V = (Int, Int) -- Vertex

type E = (V, [(Int, V)]) -- Edge: V -> (Length, V)

neighbors1, neighbors2 :: Map V Char -> V -> [V]
neighbors1 a (r, c) =
  case Map.lookup (r, c) a of
    Nothing -> []
    Just '#' -> []
    Just ch -> mapMaybe (uncurry f) $ zip [(-1, 0), (0, 1), (1, 0), (0, -1)] [".^", ".>", ".v", ".<"]
  where
    f (dr, dc) allowed
      | maybe False (`elem` allowed) (Map.lookup (r + dr, c + dc) a) =
          return (r + dr, c + dc)
      | otherwise = fail "You shall not pass!"
neighbors2 a (r, c) =
  case Map.lookup (r, c) a of
    Nothing -> []
    Just '#' -> []
    Just ch -> mapMaybe (uncurry f) $ zip [(-1, 0), (0, 1), (1, 0), (0, -1)] [".^v", ".<>", ".^v", ".<>"]
  where
    f (dr, dc) allowed
      | maybe False (`elem` allowed) (Map.lookup (r + dr, c + dc) a) =
          return (r + dr, c + dc)
      | otherwise = fail "You shall not pass!"

dfs :: (V -> [(Int, V)]) -> (Int, V) -> [(Int, V)]
dfs adj = go mempty
  where
    go visited (n, v)
      | v `elem` visited = []
      | otherwise = (n, v) : concatMap (go visited' . first (+ n)) (adj v)
      where
        visited' = Set.insert v visited

{-
By inspection: grid is a maze. "Compress" it by: while there is only one
neighbor except for the previous node, it to an edge.
 -}
compressPath :: (V -> [V]) -> V -> [(Int, V)]
compressPath adj v0 = map (snd . until done step . (v0,) . (stepLength,)) (adj v0)
  where
    stepLength = 1
    -- Stop when there is not a single unique step forward
    done (prev, (n, curr)) = length (filter (/= prev) (adj curr)) /= 1
    step (prev, (n, curr)) =
      let [next] = filter (/= prev) (adj curr)
       in (curr, (n + stepLength, next))

graphFromEdges :: (V -> [(Int, V)]) -> V -> [E]
graphFromEdges adj v0 = concat $ unfoldr (uncurry f) (mempty, [v0])
  where
    f visited (filter (`notElem` visited) -> vs) =
      case map (\v -> (v, adj v)) vs of
        [] -> Nothing
        es ->
          let visited' = Set.union visited (Set.fromList vs)
              vs' = map snd (concatMap snd es)
           in Just (es, (visited', vs'))

main = do
  input <- readFile "input"
  let (bounds@(_, (maxrow, maxcol)), grid) = parse input
      start = head [(0, c) | c <- [0 .. maxcol], Map.lookup (0, c) grid == Just '.']
      end = head [(maxrow, c) | c <- [0 .. maxcol], Map.lookup (maxrow, c) grid == Just '.']
  let longestPath adj v = maximum $ map fst $ filter ((== end) . snd) $ dfs adj (0, v)
  -- Part 1: naive
  print $ longestPath (map (1,) . neighbors1 grid) start

  {- Part 1: better
  let graph1 = Map.fromList $ graphFromEdges (compressPath (neighbors1 grid)) start
      adj1 = fromMaybe [] . flip Map.lookup graph1
  print $ longestPath adj1 start
  -}

  -- Part 2
  let graph2 = Map.fromList $ graphFromEdges (compressPath (neighbors2 grid)) start
      adj2 = fromMaybe [] . flip Map.lookup graph2
  print $ longestPath adj2 start
