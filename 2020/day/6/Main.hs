import qualified Data.Set as Set

splitGroups = foldr f [[]] . lines
    where
        f "" (g:groups) = []:g:groups
        f x  (g:groups) = (x:g):groups

main = do
    input <- readFile "input.txt"
    let groups = splitGroups input

    -- Part 1
    print $ sum $ map (length . Set.fromList . concat) groups

    -- Part 2
    print $ sum
          $ map length
          $ map (\g -> let y = Set.fromList (concat g)
                       in foldr (Set.intersection . Set.fromList) y g)
          $ groups