import Data.List
import Data.Maybe


testInput1 = unlines [
    "abcdef",
    "bababc",
    "abbcde",
    "abcccd",
    "aabcdd",
    "abcdee",
    "ababab"]

checksum = product . map length . group . sort . concat . map checksum1 . lines

checksum1 = nub . filter (`elem` [2, 3]) . map length . group . sort

testInput2 = unlines [
    "abcde",
    "fghij",
    "klmno",
    "pqrst",
    "fguij",
    "axcye",
    "wvxyz"]

difference s t = length $ filter (== False) $ zipWith (==) s t

similar = catMaybes . concatMap f . tails . lines
  where f []     = []
        f (x:xs) = map (\y -> if difference x y == 1 then return (x, y) else fail "too different") xs

main = do
    input <- readFile "input.txt"

    -- Part 1
    print $ checksum input

    -- Part 2
    let (x, y):_ = similar input
        common = map fst $ filter (uncurry (==)) $ zip x y
    putStrLn common
