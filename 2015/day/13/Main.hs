{-# LANGUAGE ViewPatterns #-}
import Control.Monad
import Data.Maybe
import Data.List

parse :: String -> ((String, String), Int)
parse (words -> [x, "would", (sign -> f), (read -> points), "happiness", "units", "by", "sitting", "next", "to", break (== '.') -> (y, _)]) =
    ((x, y), f points)

sign "lose" = negate
sign "gain" = id

vertices = nub . concatMap (\((x, y), p) -> [x, y])

happiness graph (x, y) = maybe 0 id (lookup (x, y) graph) + maybe 0 id (lookup (y, x) graph)

arrangement (x:xs) = zip (x:xs) (xs ++ [x])

arrangementHappiness graph = sum . map (happiness graph) . arrangement

main = do
    input <- readFile "input.txt"
    let graph = map parse (lines input)

    -- Part 1
    print $ maximum $ map (arrangementHappiness graph) $ permutations (vertices graph)

    -- Part 2
    print $ maximum $ map (arrangementHappiness graph) $ permutations ("yourself" : vertices graph)