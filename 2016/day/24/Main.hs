{-# LANGUAGE ViewPatterns #-}
import Data.Array
import Control.Arrow ((&&&))
import Data.Char
import Data.List
import Data.Ord
import Data.Maybe
import Data.Map (Map)
import qualified Data.Map as Map
import Data.HashSet (HashSet)
import qualified Data.HashSet as Set

parse :: String -> Array (Int, Int) Char
parse s = listArray ((0, 0), (h - 1, w - 1)) (concat xs)
  where
    xs@(x:_) = lines s
    w = length x
    h = length xs

test = unlines [
    "###########",
    "#0.1.....2#",
    "#.#######.#",
    "#4.......3#",
    "###########"
  ]

maze :: Array (Int, Int) Char
maze = listArray ((0, 0), (4, 10)) (concat (lines test))

-- BFS to get the shortest distance between x and y
alldistance :: Array (Int, Int) Char -> (Int, Int) -> [((Int, Int), Int)]
alldistance maze src = map (minimumBy (comparing snd)) . group . sortBy (comparing fst) . concat $ search' [(src, 0)] Set.empty
  where
    inside = inRange (bounds maze)
    walkable x = maze ! x /= '#'
    vertex x = isDigit (maze ! x)
    search' [] _ = [[]]
    search' ps visited = filter (vertex . fst) ps : search' ps' visited'
      where
        unvisited = not . flip Set.member visited
        ps' = [ ((x', y'), n + 1)
              | ((x, y), n) <- ps
              , (dx, dy) <- [(1, 0), (0, 1), (-1, 0), (0, -1)]
              , let (x', y') = (x + dx, y + dy)
              , inside (x', y')
              , walkable (x', y')
              , unvisited (x', y')
              ]
        visited' = visited `Set.union` Set.fromList (map fst ps')

-- BFS to get the shortest distance between x and y
distance :: Array (Int, Int) Char -> (Int, Int) -> (Int, Int) -> Int
distance maze src dst = search' [(src, 0)] Set.empty
  where
    inside = inRange (bounds maze)
    walkable x = maze ! x /= '#'
    search' ps visited
        | ((_, n):_) <- filter ((== dst) . fst) ps = n
        | otherwise                                = search' ps' visited'
      where
        unvisited = not . flip Set.member visited
        ps' = [ ((x', y'), n + 1)
              | ((x, y), n) <- ps
              , (dx, dy) <- [(1, 0), (0, 1), (-1, 0), (0, -1)]
              , let (x', y') = (x + dx, y + dy)
              , inside (x', y')
              , walkable (x', y')
              , unvisited (x', y')
              ]
        visited' = visited `Set.union` Set.fromList (map fst ps')

distances :: Array (Int, Int) Char -> Array (Char, Char) Int
distances maze = array ((a, a), (b, b)) $
      [ ((v, v), 0) | (p, v) <- vs ]
      ++
      [ z
      | (i, (p1, v1)) <- zip [1..] vs
      , (p2, v2) <- drop i vs
      , let dist = distance maze p1 p2
      , z <- [((v1, v2), dist), ((v2, v1), dist)]
      ]
  where
    vs = filter (isDigit . snd) . assocs $ maze
    es = map snd vs
    a = minimum es
    b = maximum es

alldistances :: Array (Int, Int) Char -> Array (Char, Char) Int
alldistances maze = array ((a, a), (b, b)) $
      [ ((v, v), 0) | (p, v) <- vs ]
      ++
      [ ((v1, maze ! p2), d)
      | (p1, v1) <- vs
      , (p2, d) <- alldistance maze p1
      ]
  where
    vs = filter (isDigit . snd) . assocs $ maze
    es = map snd vs
    a = minimum es
    b = maximum es

shortestPath :: Array (Int, Int) Char -> (Int, [Char])
shortestPath maze = minimumBy (comparing fst) $ map (pathLength &&& id) paths
  where
    -- Distance matrix
    dist = distances maze

    pathLength vs = sum $ zipWith (curry (dist !)) vs (tail vs)

    -- Get all permutations of vertices (starting on vertex 0)
    vs = let (vs, ws) = unzip (indices dist) in nub vs `union` nub ws
    paths = map ('0':) $ permutations (vs \\ ['0'])

-- main = readFile "input.txt" >>= print . distances . parse

main = readFile "input.txt" >>= print . length . flip alldistance (5, 135) . parse
