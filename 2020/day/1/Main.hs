import qualified Data.IntSet as Set

main = do
    input <- readFile "input.txt"
    let numbers = Set.fromList $ map read $ lines input

    -- Part 1
    print $ head [ x * (2020 - x)
                 | x <- Set.toList numbers
                 , (2020 - x) `Set.member` numbers
                 ]

    -- Part 2
    print $ head [ x * y * (2020 - x - y)
                 | x <- Set.toList numbers
                 , y <- Set.toList numbers
                 , (2020 - x - y) `Set.member` numbers
                 ]