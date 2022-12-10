{-# LANGUAGE ViewPatterns #-}
import Data.List
import Data.Foldable
import Debug.Trace
import Control.Arrow

execute :: [String] -> (Int, [Int])
execute = second concat . mapAccumL f 1

f :: Int -> String -> (Int, [Int])
f x "noop" = (x, [x])
f x (stripPrefix "addx " -> Just (read -> dx)) = (x + dx, [x, x])
f x _ = error "bad input"

main = do
    input <- readFile "input.txt"
    let (x, signal) = execute (lines input)
    
    -- Part 1
    print $ sum $ map (\cycle -> cycle * (signal !! (cycle - 1))) [20, 60, 100, 140, 180, 220]