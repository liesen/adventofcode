import Data.List
import Data.List.Split

--- Day 18: Like a Rogue ---
read1 :: Char -> Int
read1 '^' = 0  -- 
read1 '.' = 1  -- Safe

step :: [Int] -> [Int]
step (0:0:1:zs) = 0:step (0:1:zs)
step (1:0:0:zs) = 0:step (0:0:zs)
step (0:1:1:zs) = 0:step (1:1:zs)
step (1:1:0:zs) = 0:step (1:0:zs)
step (x:y:z:zs) = 1:step (y:z:zs)
step _          = []

generate :: String -> [[Int]]
generate = iterate f . map read1
  where
    f s = step (1:s ++ [1])

test1 = sum . concat . take 10 . generate $ ".^^.^.^^^^"

main1 = readFile "input.txt" >>= print . sum . concat . take 40 . generate . head . lines

--- Part Two ---
main2 = readFile "input.txt" >>= print . sum . map sum . take 400000 . generate . head . lines

main = main1 >> main2
