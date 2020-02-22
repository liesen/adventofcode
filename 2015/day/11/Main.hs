import Data.Char
import Data.List

input = "hepxcrrq"

valid s = and [ any (\(x, y, z) -> succ x == y && succ y == z) $ zip3 s (tail s) (tail (tail s))
              , all (`notElem` "iol") s
              , length (nub $ filter (\(x, y) -> x == y) $ zip s (tail s)) >= 2
              ]

increment :: String -> String
increment = adjust . foldr f ([], 1)
  where
    f c (s, carry) = let (carry', c') = (ord c - ord 'a' + carry) `divMod` (ord 'z' - ord 'a' + 1)
                     in (chr (c' + ord 'a') : s, carry')
    
    adjust (s, 0) = s
    adjust (s, 1) = 'a' : s

next = until valid increment . increment                  

main = do
    -- Part 1
    let ans1 = next input
    putStrLn ans1

    -- Part 2
    putStrLn $ next ans1