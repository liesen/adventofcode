{-# LANGUAGE ViewPatterns #-}
import Data.List
import Data.List.Split

type P2 = (Int, Int)
type Line = (P2, P2)

parseLine :: String -> [Line]
parseLine = parseLine' (0, 0) . splitOn ","

parseLine' :: (Int, Int) -> [String] -> [Line]
parseLine' _        []                     = []
parseLine' (x0, y0) (('U':(read -> n)):xs) = let p@(_, q) = ((x0, y0), (x0, y0 - n)) in p : parseLine' q xs
parseLine' (x0, y0) (('D':(read -> n)):xs) = let p@(_, q) = ((x0, y0), (x0, y0 + n)) in p : parseLine' q xs
parseLine' (x0, y0) (('L':(read -> n)):xs) = let p@(_, q) = ((x0, y0), (x0 - n, y0)) in p : parseLine' q xs
parseLine' (x0, y0) (('R':(read -> n)):xs) = let p@(_, q) = ((x0, y0), (x0 + n, y0)) in p : parseLine' q xs

parse :: String -> ([Line], [Line])
parse input = let [a, b] = map parseLine (lines input) in (a, b)

intersections :: [Line] -> [Line] -> [P2]
intersections as bs =
    -- Horizontal a, vertical b
    [ (bx0, ay0)
    | a@((ax0, ay0), (ax1, ay1)) <- as, ay0 == ay1
    , b@((bx0, by0), (bx1, by1)) <- bs, bx0 == bx1
    , min ax0 ax1 <= bx0 && max ax0 ax1 >= bx0
    , min by0 by1 <= ay0 && max by0 by1 >= ay0
    , bx0 /= 0 && ay0 /= 0
    ]
    ++
    -- Vertical a, horizontal b
    [ (ax0, by0)
    | a@((ax0, ay0), (ax1, ay1)) <- as, ax0 == ax1
    , b@((bx0, by0), (bx1, by1)) <- bs, by0 == by1
    , min ay0 ay1 <= by0 && max ay0 ay1 >= by0
    , min bx0 bx1 <= ax0 && max bx0 bx1 >= ax0
    , ax0 /= 0 && by0 /= 0
    ]

dist0 (x1, y1) = abs x1 + abs y1

main = do
    input <- readFile "input.txt"

    let (as, bs) = parse input
        ans1 = minimum $ map dist0 (intersections as bs)
    print ans1