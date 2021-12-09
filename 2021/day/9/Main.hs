{-# LANGUAGE RecordWildCards, ViewPatterns, TupleSections #-}
import Data.Char
import Data.List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Sequence (Seq(..))
import qualified Data.Sequence as Seq
import Data.Set (Set)
import qualified Data.Set as Set

neighbors (x, y) = [(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)]

lowPoints heightmap = Map.filterWithKey lowPoint heightmap
  where
    lowPoint p h = all (> h) (mapMaybe (`Map.lookup` heightmap) (neighbors p))

bfs :: Ord r => (a -> r) -> (a -> [(a, Int)]) -> a -> [(a, Int)]
bfs rep next start = loop Set.empty (Seq.fromList [(start, 0)])
  where
    loop _    Empty = []
    loop seen ((x, cost) :<| q1)
        | Set.member r seen = loop seen q1
        | otherwise = (x, cost) : loop seen1 q2
        where
          r = rep x
          seen1 = Set.insert r seen
          q2 = q1 <> Seq.fromList (map (\(x', stepcost) -> (x', cost + stepcost)) (next x))

basin heightmap p = bfs id (next heightmap) p

next heightmap p@(r, c) =
    case Map.lookup p heightmap of
        Nothing -> []
        Just 9 -> []
        Just h -> do
            q <- neighbors p

            case Map.lookup q heightmap of
                Nothing -> fail ""
                Just 9 -> fail ""
                Just i -> if i > h then return (q, 1) else fail ""

main = do
    input <- readFile "input.txt"

    let heightmap :: Map (Int, Int) Int
        heightmap = Map.fromList $ do
            (r, row) <- zip [0..] (lines input)
            (c, col) <- zip [0..] row 
            return ((r, c), digitToInt col)

    -- Part 1
    print $ sum $ (+ 1) <$> lowPoints heightmap

    -- Part 2
    print $ product $ take 3 $ reverse $ sort $ map (length . basin heightmap) $ Map.keys (lowPoints heightmap)
