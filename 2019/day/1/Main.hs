import Data.Monoid

fuel x = (x `div` 3) - 2

fuelfuel = sum . takeWhile (> 0) . tail . iterate fuel

main = do
    input <- readFile "input.txt"
    
    -- Part 1
    print $ getSum . foldMap (Sum . fuel . read) $ lines input

    -- Part 2
    print $ getSum . foldMap (Sum . fuelfuel . read) $ lines input