{-# LANGUAGE ViewPatterns #-}
import Control.Monad
import Data.Char
import Data.Function
import Data.List
import Data.Ord
import Text.ParserCombinators.ReadP
import Text.Printf

parseLine :: ReadP (Char, Char)
parseLine = do
    string "Step "
    x <- get
    string " must be finished before step "
    y <- get 
    string " can begin."
    return (x, y)

parse = sepBy parseLine (char '\n') <* (char '\n' >> eof)

incoming, outgoing :: Ord a => a -> [(a, a)] -> [a]
incoming vx edges = [v | (v, w) <- edges, w == vx]
outgoing vx edges = [w | (v, w) <- edges, v == vx]

order []             _     visited = reverse visited
order (sort -> v:vs) edges visited =
    let ws = [u | u <- outgoing v edges
                , all (`elem` (v:visited)) (incoming u edges)]
    in order (ws ++ vs) edges (v:visited)

debug :: Int -> Int -> [(Int, Char)] -> [Char] -> String
debug n t workers done =
    (show t) ++ "\t" ++
    (concatMap (\(_, c) -> [c, '\t']) (take n (workers ++ replicate n (0, '.')))) ++
    done

schedule :: [(Char, Char)]  -- Edges of dependency graph
         -> Int -- Number of workers
         -> Int -- Time
         -> [(Int, Char)] -- Active workers
         -> [Char] -- Available steps (nodes)
         -> [Char] -- Completed steps
         -> (Int, [Char]) -- (Time of completion, steps completed)
schedule edges n t [] []                  visited = (t, visited)
schedule edges n t [] (sort -> available) visited =
    let new = take n (map (start t) available)
        available' = drop n available
    in schedule edges n t new available' visited
schedule edges n t workers available      visited =
    let (finished:(concat -> running)) = groupBy ((==) `on` fst) (sort workers)
        (t', _) = head finished
        visited' = visited ++ map snd finished
        available' = sort $ available ++ [u | (_, v) <- finished, u <- outgoing v edges, all (`elem` visited') (incoming u edges)]
        new = take (n - length running) (map (start t') available')
        available'' = drop (n - length running) available'
    in schedule edges n t' (running ++ new) available'' visited'

start t v = (t + ord v - ord 'A' + 61, v)

main = do
    input <- readFile "input.txt"
    let [(edges, "")] = readP_to_S parse input

    -- Part 1
    let vxs = sort $ nub [u | (u, v) <- edges, null (incoming u edges)]
        s = order vxs edges ""
    putStrLn s

    -- Part 2
    -- Sanity check: schedule edges 1 0 [] vxs [] == order vxs edges ""
    let (t, s) = schedule edges 5 0 [] vxs ""
    print t
