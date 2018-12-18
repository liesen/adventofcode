{-# LANGUAGE ViewPatterns #-}
import Data.List
import Data.Array


parse = listArray ((0, 0), (49, 49)) . concat . transpose . lines

showArea a = unlines [[a ! (x, y) | x <- [x1..x2]] | y <- [y1..y2]]
  where bnds@((x1, y1), (x2, y2)) = bounds a

step a = array bnds (map change (range bnds))
  where
    bnds = bounds a

    -- Neighbors
    nb (x, y) = filter (inRange bnds) $
                      [(x - 1, y - 1), (  x  , y - 1), (x + 1, y - 1),
                       (x - 1, y)    ,                 (x + 1, y),
                       (x - 1, y + 1), (  x  , y + 1), (x + 1, y + 1)]
        
    change p | a ! p == '.' =
        if length (filter ((== '|') . (a !)) (nb p)) >= 3
            then (p, '|')
            else (p, a ! p)
    change p | a ! p == '|' =
        if length (filter ((== '#') . (a !)) (nb p)) >= 3
            then (p, '#')
            else (p, a ! p)
    change p | a ! p == '#' =
        if length (filter ((== '#') . (a !)) (nb p)) >= 1
           && length (filter ((== '|') . (a !)) (nb p)) >= 1
            then (p, '#')
            else (p, '.')

score = product . map length . group . sort . filter (`elem` "|#") . elems

main = do
    input <- readFile "input.txt"
    let area = parse input
    
    -- Part 1
    print $ score $ iterate step area !! 10

    -- Part 2
    -- The score repeats itself every 7000 thousand minutes
    print $ score $ iterate step area !! (1000000000 `mod` 7000)
