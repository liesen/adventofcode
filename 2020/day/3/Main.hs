import Control.Monad

treeCount g dx dy = length $ filter tree [0..(length g - 1) `div` dy]
  where
    tree r = let y = r * dy
                 x = r * dx
             in cycle (g !! y) !! x == '#'

main = do
    input <- readFile "input.txt"

    -- Part 1
    print $ treeCount (lines input) 3 1

    -- Part 2
    print $ product $ zipWith (treeCount (lines input)) [1, 3, 5, 7, 1] [1, 1, 1, 1, 2]