import Data.Char

digits :: Integer -> [Int]
digits = map digitToInt . show

pattern i = tail $ cycle $ concat [replicate i 0, replicate i 1, replicate i 0, replicate i (-1)]

phase i ds = abs (sum (zipWith (*) ds (pattern i))) `mod` 10

step :: [Int] -> [Int]
step ds = map (\(d, ph) -> phase ph ds) (zip ds [1..])

main = do
    input <- (head . lines) <$> readFile "input.txt"
    let digits = map digitToInt input

    -- Part 1
    putStrLn (map intToDigit (take 8 (iterate step digits !! 100)))
