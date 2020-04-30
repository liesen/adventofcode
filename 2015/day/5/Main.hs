import Data.List

nice1 s = and [ length (filter (`elem` "aeiou") s) >= 3
              , or (zipWith (==) s (tail s))
              , all (`notElem` [('a', 'b'), ('c', 'd'), ('p', 'q'), ('x', 'y')]) (zip s (tail s))
              ]

{-
any (> 1) (map length (group (sort (zip s (tail s)))))
              , all (\(x, y, z) -> not (x == y && y == z)) (zip3 s (tail s) (tail (tail s)))
              , 
-}
overlapping s = concatMap (\(x, y, z) -> if x == y && y == z then replicate 2 (x, y) else []) (zip3 s (tail s) (tail (tail s)))
pairs s = zip s (tail s)

f s = pairs s \\ overlapping s

nice2 s = and [ any (> 1) $ map length $ group $ sort $ f s
              , any (\(x, y, z) -> x == z) (zip3 s (tail s) (tail (tail s)))
              ]

test2 = [
    nice2 "qjhvhtzxzqqjkmpb"
  , nice2 "xxyxx"
  , not (nice2 "uurcxstgmygtbstg")
  , not (nice2 "ieodomkazucvgmuy")
  ]

main = do
    input <- readFile "input.txt"

    -- Part 1
    print $ length $ filter nice1 $ lines input

    -- Part 2
    print $ length $ filter nice2 $ lines input
