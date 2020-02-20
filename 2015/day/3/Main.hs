import Data.List

step (y, x) '^' = (y - 1, x)
step (y, x) 'v' = (y + 1, x)
step (y, x) '<' = (y, x - 1)
step (y, x) '>' = (y, x + 1)

robosteps (a, b) (x:y:ys) = (a, b) : robosteps (step a x, step b y) ys
robosteps (a, b) _        = (a, b) : []

main = do
    input <- readFile "input.txt"

    -- Part 1
    print $ length . sort . nub $ scanl step (0, 0) input

    -- Part 2
    print $ length . sort . nub $ concatMap (\(a, b) -> [a, b]) $ robosteps ((0, 0), (0, 0)) input
