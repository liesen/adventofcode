step n '(' = n + 1
step n ')' = n - 1

main = do
    input <- readFile "input.txt"

    -- Part 1
    print $ foldl step 0 input

    -- Part 2
    print $ length $ takeWhile (/= -1) $ scanl step 0 input
