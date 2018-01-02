{-# LANGUAGE ViewPatterns #-}
module Main where

import Data.Array
import Data.List hiding (delete)
import Data.Ord

type Port = Int
type Component = (Port, Port)
type Bridge = [Component]

-- How many components of type (a, b) (or (b, a)) we've got
type Table = Array Component Int

parseComponent :: String -> Component
parseComponent (break (== '/') -> (a, '/':b)) = (read a, read b) 

delete :: Component -> Table -> Table
delete (a, b) x = accum (-) x [((a, b), 1), ((b, a), 1)]

search :: Table -> Component -> [Bridge]
search x k@(a, b)
    | x ! k <= 0 = [[]]
    | otherwise  = do
        c <- [mn..mx]
        cs <- search (delete k x) (b, c)
        return (k:cs)
  where
    ((mn, _), (mx, _)) = bounds x

strength :: Bridge -> Int
strength = sum . map (\(a, b) -> a + b)

main = do
    input <- readFile "input.txt"
    let components = map parseComponent $ lines input
        ports = concatMap (\(a, b) -> [a, b]) components
        mx = maximum ports
        mn = minimum ports
        x = accumArray (+) 0 ((mn, mn), (mx, mx)) $ concatMap (\(a, b) -> [((a, b), 1), ((b, a), 1)]) components
        components0 = filter ((== 0) . fst) components
    
    -- Part 1
    print $ maximum $ map strength $ map (maximumBy (comparing strength) . search x) components0

    -- Part 2
    let longest = maximumBy (comparing length)
    print $ maximum $ map strength $ map (longest . search x) components0
