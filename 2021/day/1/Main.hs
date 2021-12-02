solve :: [Int] -> Int
solve measurements = sum $ zipWith (\a b -> if b > a then 1 else 0) measurements (tail measurements)

main = do
    input <- readFile "input.txt"
    let measurements = map read (lines input)

    -- Part 1
    print $ solve measurements

    -- Part 2
    print $ solve $ zipWith3 (\a b c -> a + b + c) (measurements) (drop 1 measurements) (drop 2 measurements)
