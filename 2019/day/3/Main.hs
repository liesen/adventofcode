{-# LANGUAGE ViewPatterns #-}
import Data.List
import Data.List.Split

type P2 = (Int, Int)

data Segment = H Int P2 | V Int P2
    deriving Show

parse :: String -> ([Segment], [Segment])
parse input = let [a, b] = map parseSegment (lines input) in (a, b)

parseSegment = parseLine' (0, 0) . splitOn ","

parseLine' :: (Int, Int) -> [String] -> [Segment]
parseLine' _        []                     = []
parseLine' (x0, y0) (('U':(read -> n)):xs) = let p@(_, q) = ((x0, y0), (x0, y0 - n)) in V x0 (y0, y0 - n) : parseLine' q xs
parseLine' (x0, y0) (('D':(read -> n)):xs) = let p@(_, q) = ((x0, y0), (x0, y0 + n)) in V x0 (y0, y0 + n) : parseLine' q xs
parseLine' (x0, y0) (('L':(read -> n)):xs) = let p@(_, q) = ((x0, y0), (x0 - n, y0)) in H y0 (x0, x0 - n) : parseLine' q xs
parseLine' (x0, y0) (('R':(read -> n)):xs) = let p@(_, q) = ((x0, y0), (x0 + n, y0)) in H y0 (x0, x0 + n) : parseLine' q xs

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

main = do
    input <- readFile "input.txt"

    let (as, bs) = parse input
        ans1 = minimum $ map dist0 (intersections as bs)
    print ans1