import Control.Applicative
import qualified Data.Map.Strict as Map

--- Day 19: An Elephant Named Joseph ---
input = 3005290

elves n = zip [1..n] (replicate n 1)

play = go 1 . Map.fromList . elves
  where
    go i m =
        let Just (j, v) = Map.lookupGE i m <|> Map.lookupGT 0 m
            Just (k, w) = Map.lookupGT j m <|> Map.lookupGT 0 m
        in if j == k
            then (j, v + w)
            else go k (Map.delete k (Map.insert j (v + w) m))

--- Part Two ---
main = do
    -- Part 1
    let (i, n) = play input
    print i

    -- Part 2
