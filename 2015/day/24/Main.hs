import Data.List (sortBy)
import Data.Ord

group 0 []     = [[]]
group _ []     = []
group n (x:xs) = map (x :) (group (n - x) xs) ++ group n xs

main = do
    input <- readFile "input.txt"
    let packages = map read (lines input)
        groupSum = sum packages `div` 3
    
    -- Part 1
    -- Simply find the smallest group of packages (by sorting
    -- from largest to smallest) summing to {all packages} / 3
    print $ product $ head $ group groupSum $ sortBy (comparing Down) packages