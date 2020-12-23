{-# LANGUAGE ImportQualifiedPost #-}
import Data.List
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe

data Seats = Seats (Int, Int) (Map (Int, Int) Char)
  deriving (Eq)

parse :: (Int, Int) -> String -> Seats
parse (y, x) "\n"      = Seats (y, x - 1) mempty
parse (y, x) ('\n':xs) = parse (y + 1, 0) xs
parse (y, x) ('.':xs)  = parse (y, x + 1) xs
parse (y, x) ('L':xs)  = let Seats bnds m = parse (y, x + 1) xs in Seats bnds (Map.insert (y, x) 'L' m)
parse (y, x) ('#':xs)  = let Seats bnds m = parse (y, x + 1) xs in Seats bnds (Map.insert (y, x) '#' m)

step :: Seats -> Seats
step s@(Seats bounds m) = Seats bounds (Map.mapWithKey update m)
  where
    update p 'L' | '#' `notElem` mapMaybe (`Map.lookup` m) (neighbors p) = '#'
    update p '#' | length (filter (== '#') (mapMaybe (`Map.lookup` m) (neighbors p))) >= 4 = 'L'
    update p z = z

neighbors :: (Int, Int) -> [(Int, Int)]
neighbors (y, x) =
    [(y - 1, x - 1), (y - 1, x), (y - 1, x + 1),
     (y, x - 1),                 (y, x + 1),
     (y + 1, x - 1), (y + 1, x), (y + 1, x + 1)]

step2 :: Seats -> Seats
step2 s@(Seats bounds m) = Seats bounds (Map.mapWithKey update m)
  where
    neighbors = neighbors2 s
    update p 'L' | '#' `notElem` mapMaybe (`Map.lookup` m) (neighbors p) = '#'
    update p '#' | length (filter (== '#') (mapMaybe (`Map.lookup` m) (neighbors p))) >= 5 = 'L'
    update p z = z

neighbors2 :: Seats -> (Int, Int) -> [(Int, Int)]
neighbors2 (Seats (ymax, xmax) m) (y0, x0) =
    concat [ take 1 $ filter (`Map.member` m) [(y, x0) | y <- [y0 - 1,y0 - 2..0]]
           , take 1 $ filter (`Map.member` m) [(y, x0) | y <- [y0 + 1..ymax]]
           , take 1 $ filter (`Map.member` m) [(y0, x) | x <- [x0 - 1,x0 - 2..0]]
           , take 1 $ filter (`Map.member` m) [(y0, x) | x <- [x0 + 1..ymax]]
           , take 1 $ filter (`Map.member` m) [(y0 - d, x0 + d) | d <- [1..min y0 (xmax - x0)]]
           , take 1 $ filter (`Map.member` m) [(y0 - d, x0 - d) | d <- [1..min y0 x0]]
           , take 1 $ filter (`Map.member` m) [(y0 + d, x0 + d) | d <- [1..min (ymax - y0) (xmax - x0)]]
           , take 1 $ filter (`Map.member` m) [(y0 + d, x0 - d) | d <- [1..min (ymax - y0) x0]]
           ]

countOccupied (Seats bounds m) = length $ Map.filter (== '#') m

converge :: Eq a => (a -> a) -> a -> a
converge f x
    | x == x' = x
    | otherwise = converge f x'
  where x' = f x

main = do
    input <- readFile "input.txt"
    let seats = parse (0, 0) input

    -- Part 1
    print $ countOccupied $ converge step seats
    
    -- Part 2
    print $ countOccupied $ converge step2 seats
