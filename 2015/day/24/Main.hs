import Data.List
import Data.Ord
import Data.Function (on)

-- Find set of elements of a list summing to a value
sumGroups 0 []     = [[]]
sumGroups _ []     = []
sumGroups n (x:xs) = map (x :) (sumGroups (n - x) xs) ++ sumGroups n xs

main = do
    input <- readFile "input.txt"
    let packages = map read (lines input)
    
    -- Part 1
    print $ snd $ minimum $ map (\g -> (length g, product g)) $ sumGroups (sum packages `div` 3) packages

    -- Part 2
    print $ snd $ minimum $ map (\g -> (length g, product g)) $ sumGroups (sum packages `div` 4) packages