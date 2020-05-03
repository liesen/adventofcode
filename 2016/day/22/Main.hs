{-# LANGUAGE ViewPatterns #-}
import Control.Arrow
import Data.Array
import Data.List

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

-- Simple node: only contains Size and Used
newtype Use = Use (Int, Int)
    deriving (Eq, Ord)

instance Show Use where
    show (Use (used, size)) = show used ++ "T/" ++ show size ++ "T"

showGrid :: (Int, Int) -> Array (Int, Int) Use -> String
showGrid pos a =
    unlines [ intercalate " " [ node (x, y) | x <- [mincol..maxcol] ]
            | y <- [minrow..maxrow]]
  where
    ((mincol, minrow), (maxcol, maxrow)) = bounds a
    minSize = minimum [ size | Use (_used, size) <- elems a ]
    node pos = brackets pos (char pos)
    brackets (x, y) z
      | (x, y) == (0, 0) = ['(', z, ')']
      | otherwise        = [' ', z, ' ']
    char (x, y)
      | (x, y) == pos  = 'G'
      | used == 0      = '_'
      | used > minSize = '#'
      | otherwise      = '.'
      where
        Use (used, size) = a ! (x, y)

main = do
    input <- readFile "input.txt"
    let nodes = map parse . drop 2 . lines $ input

    -- Part 1
    print $ length $ viablePairs nodes

    -- Part 2
    -- Solved by hand
    let a = array (minimum locs, maximum locs) (map (loc &&& use) nodes)
        locs = map loc nodes
        xmax = maximum [x | (x, y) <- locs, y == 0]
        use node = Use (used node, size node)
    -- putStrLn (showGrid (xmax, 0) a)

    -- Find unused node
    let Just unused@(x0, y0) = find (\p -> let Use (used, size) = a ! p in used == 0) locs

    print $
      -- Move unused node to goal data node
      x0 + y0 + xmax
      -- Then rotate the goal data using the free node all the way to the top left node
      + 5 * (xmax - 1)