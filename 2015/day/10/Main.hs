import Data.List

input = "1321131112"

lookAndSay = foldMap (\g -> show (length g) ++ [head g]) . group

main = do
    -- Part 1
    print $ length $ iterate lookAndSay input !! 40

    -- Part 2
    print $ length $ iterate lookAndSay input !! 50