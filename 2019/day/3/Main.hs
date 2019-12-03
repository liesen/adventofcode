{-# LANGUAGE ViewPatterns #-}
import Data.List
import Data.List.Split
import Data.Monoid

type P2 = (Int, Int)

data Step
    = U Int
    | D Int
    | L Int
    | R Int
  deriving Show

data Segment
    = H Int P2
    | V Int P2
  deriving Show

parse :: String -> ([Step], [Step])
parse input = let [a, b] = map parseSteps (lines input) in (a, b)

parseSteps = parseLine' . splitOn ","

parseLine' :: [String] -> [Step]
parseLine' []                     = []
parseLine' (('U':(read -> n)):xs) = U n : parseLine' xs
parseLine' (('D':(read -> n)):xs) = D n : parseLine' xs
parseLine' (('L':(read -> n)):xs) = L n : parseLine' xs
parseLine' (('R':(read -> n)):xs) = R n : parseLine' xs

segments :: (Int, Int) -> [Step] -> [Segment]
segments _        []       = []
segments (x0, y0) (U n:xs) = V x0 (y0, y0 - n) : segments (x0, y0 - n) xs
segments (x0, y0) (D n:xs) = V x0 (y0, y0 + n) : segments (x0, y0 + n) xs
segments (x0, y0) (L n:xs) = H y0 (x0, x0 - n) : segments (x0 - n, y0) xs
segments (x0, y0) (R n:xs) = H y0 (x0, x0 + n) : segments (x0 + n, y0) xs

segments0 = segments (0, 0)

intersections :: [Segment] -> [Segment] -> [P2]
intersections as bs =
    -- Horizontal a, vertical b
    [ (bx, ay)
    | a@(H ay (ax0, ax1)) <- as
    , b@(V bx (by0, by1)) <- bs
    , min ax0 ax1 <= bx && max ax0 ax1 >= bx
    , min by0 by1 <= ay && max by0 by1 >= ay
    , bx /= 0 && ay /= 0
    ]
    ++
    -- Vertical a, horizontal b
    [ (ax, by)
    | a@(V ax (ay0, ay1)) <- as
    , b@(H by (bx0, bx1)) <- bs
    , min ay0 ay1 <= by && max ay0 ay1 >= by
    , min bx0 bx1 <= ax && max bx0 bx1 >= ax
    , ax /= 0 && by /= 0
    ]

dist0 (x1, y1) = abs x1 + abs y1

-- Step through the path to find /p/
steps p@(px, py) s0 = go s0
  where
    go (x, y) [] = error "unable to find the point on the path"
    go (x, y) (U n:xs)
        | px == x && py >= y - n && py <= y = return (y - py)
        | otherwise = return n <> go (x, y - n) xs
    go (x, y) (D n:xs)
        | px == x && py >= y && py <= y + n = return (py - y)
        | otherwise = return n <> go (x, y + n) xs
    go (x, y) (L n:xs)
        | py == y && px >= x - n && px <= x = return (x - px)
        | otherwise = return n <> go (x - n, y) xs
    go (x, y) (R n:xs)
        | py == y && px >= x && px <= x + n = return (px - x)
        | otherwise = return n <> go (x + n, y) xs

steps0 p = steps p (0, 0)

test = ([R 8, U 5, L 5, D 3], [U 7, R 6, D 4, L 4])

main = do
    input <- readFile "input.txt"

    -- Part 1
    let (as, bs) = parse input
        ixs = intersections (segments0 as) (segments0 bs)
        ans1 = minimum (map dist0 ixs)
    print ans1

    -- Part 2
    let ans2 = getSum $ minimum (map (\ix -> (steps0 ix as) <> (steps0 ix bs)) ixs)
    print ans2
