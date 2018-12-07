import Text.ParserCombinators.ReadP
import Data.List

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

merge []     ys     = ys
merge xs     []     = xs
merge (x:xs) (y:ys) | y < x     = y : merge (x:xs) ys
merge (x:xs) (y:ys) | otherwise = x : merge xs (y:ys)

order []             _     visited = reverse visited
order (v:vs)         edges visited =
    let ws = [u | u <- outgoing v edges
                , all (`elem` (v:visited)) (incoming u edges)]
    in order (merge (sort ws) vs) edges (v:visited)

main = do
    input <- readFile "input.txt"
    let [(edges, "")] = readP_to_S parse input

    -- Part 1
    let vxs = sort $ nub [u | (u, v) <- edges, null (incoming u edges)]
    putStrLn $ order vxs edges ""
    print $ order (sort vxs) edges "" == "BETUFNVADWGPLRJOHMXKZQCISY"
