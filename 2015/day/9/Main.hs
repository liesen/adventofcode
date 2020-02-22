{-# LANGUAGE ViewPatterns #-}
import Data.List
import Data.Maybe
import Data.Monoid

parse :: String -> ((String, String), Int)
parse (words -> [src, "to", dst, "=", read -> dist]) = ((src, dst), dist)

route xs = zip xs (tail xs)

-- Distance between two cities
distance graph edge = lookup edge graph

-- Distance of the route between a list of cities
routeDistance graph = fmap getSum . mconcat . map (fmap Sum . distance graph) . route

-- Make graph undirected by adding reverse edges
undirect graph = graph ++ map (\((src, dst), dist) -> ((dst, src), dist)) graph

main = do
    input <- readFile "input.txt"

    let graph = undirect $ map parse $ lines input
        cities = nub $ concatMap (\((src, dst), _) -> [src, dst]) graph

    -- Part 1
    print $ minimum $ catMaybes $ map (routeDistance graph) $ permutations cities

    -- Part 2
    print $ maximum $ catMaybes $ map (routeDistance graph) $ permutations cities