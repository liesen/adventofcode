import Data.List

--- Part One ---
test :: [Int] -> Bool
test [x, y, z] = and [x + y > z, x + z > y, y + z > x]
test _         = False

count = length . filter test

parse1 :: String -> [[Int]]
parse1 = map (map read . words) . lines

run1 :: String -> Int
run1 = count . parse1

main1 = readFile "input.txt" >>= print . run1

--- Part Two ---
parse2 :: String -> [[Int]]
parse2 = chunk . concat . transpose . parse1
  where
    chunk (x:y:z:xs) = [x, y, z] : chunk xs
    chunk []         = []

run2 = count . parse2

main2 = readFile "input.txt" >>= print . run2

main = main2
