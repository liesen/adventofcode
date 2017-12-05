module Main where

import Data.List
import Data.Maybe
import Data.Map (Map)
import qualified Data.Map as Map

input = 277678

{- First solved by hand:

-- side for ring = 527 (closest odd number less than sqrt input)
-- ring = 263 (527 is the 263th odd number)
-- max number in ring 263 = 527^2 = 277729
-- distance to last number in ring = 277678 - 277729 = 51  -- not too far off = in the "south"
-- distance from the center of the row = (527 - 1) / 2 - 51 = 212
-- manhattan distance to (0,0) = (263 - 0) + (212 - 0) = 475
-}

-- From https://stackoverflow.com/a/947447, adapted to yield an infinite spiral
spiral = scanl (\(a,b) (c,d) -> (a+c,b+d)) (0,0) $
         concat [ (:) (1,0) . tail 
                $ concatMap (replicate n) [(0,1),(-1,0),(0,-1),(1,0)]
                | n <- [2,4..] ]

manhattan (x, y) = abs x + abs y

main1 = print $ manhattan (spiral !! input) - 1

update env (x,y) = 
    let value = sum $ mapMaybe (\(dx,dy) -> Map.lookup (x+dx, y+dy) env) neighbors
    in ((Map.insert (x,y) value env), value)
  where
    neighbors = [(-1,-1), ( 0,-1), ( 1,-1),
                 (-1, 0),          ( 1, 0),
                 (-1, 1), ( 0, 1), ( 1, 1)]

main2 = print $ head $ dropWhile (<= input) $ snd $ mapAccumL update (Map.singleton (0, 0) 1) (tail spiral)

main = do
    main1
    main2
