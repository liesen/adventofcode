import Data.List
import Data.Ord
import Data.Function (on)

pack :: Int -> [Int] -> [[Int]]
pack 0 _      = [[]]
pack n []     = []
pack n (x:xs) = pack n xs ++ if x <= n then map (x :) (pack (n - x) xs) else []

main = do
    input <- readFile "input.txt"
    let containers = map read (lines input)

    -- Part 1
    print $ length $ pack 150 containers

    -- Part 2
    print $ length $ head $ groupBy ((==) `on` length) $ sortBy (comparing length) $ pack 150 containers