import Data.Array
import Data.Char
import Data.List

dirs = [(1, 0), (-1, 0), (0, 1), (0, -1)]

-- Reverse direction: never go backwards!
rev (1, 0) = (-1, 0)
rev (0, 1) = (0, -1)
rev (-1, 0) = (1, 0)
rev (0, -1) = (0, 1)

-- Find path
search :: (Int, Int) -> Array (Int, Int) Char -> (Int, Int) -> [((Int, Int), (Int, Int))]
search p a dir = do
    q@(x, y) <- search' p a dir
    let path = concatMap (\dir@(dx, dy) -> search (x + dx, y + dy) a dir) (dirs \\ [rev dir])
    (p, q) : path
  where
     -- Find next junction or letter
     search' :: (Int, Int) -> Array (Int, Int) Char -> (Int, Int) -> [(Int, Int)]
     search' p@(x, y) a (dx, dy)
         | not (inRange (bounds a) p)      = fail "out of bounds"
         | a ! p == ' '                    = fail "out of space"
         | a ! p == '-' || a ! p == '|'    = search' (x + dx, y + dy) a (dx, dy)
         | isUpper (a ! p) || a ! p == '+' = return p

parse :: String -> Array (Int, Int) Char
parse s = listArray ((0, 0), (nrows - 1, ncols - 1)) elems
  where
    nrows = length (lines s)
    ncols = maximum (map length (lines s))
    elems = concat (lines s)

main = do
    input <- readFile "input.txt"
    let Just x = findIndex (== '|') (head (lines input))
        entry = (0, x)
        grid = parse input
        edges = search entry grid (1, 0)

    -- Part 1
    putStrLn $ filter isUpper $ concatMap (\(p, q) -> [grid ! p, grid ! q]) edges

    -- Part 2
    let dist (a, b) (c, d) = abs (a - c) + abs (b - d)
        totalDist = sum $ map (+ 1 {- every edge is 1 short because it excludes the starting vertex -}) $ map (uncurry dist) edges
    print totalDist
