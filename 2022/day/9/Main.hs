{-# LANGUAGE ImportQualifiedPost, ViewPatterns #-}
import Control.Arrow (second)
import Control.Monad
import Data.Set qualified as Set
import Data.Traversable


type Rope = [(Int, Int)]

-- Work out where the tail goes based on the (updated) head
adjust :: (Int, Int) -> (Int, Int) -> (Int, Int)
adjust (hy, hx) (ty, tx)
    | abs dy <= 1 && abs dx <= 1 = (ty, tx)
    | abs dy >= 2 && abs dx >= 2 = (ty', tx')
    | abs dy >= 2 = (ty', hx)
    | abs dx >= 2 = (hy, tx')
    | otherwise = (tx, ty)
    where
        dy = hy - ty
        dx = hx - tx
        ty'
            | ty < hy = hy - 1
            | otherwise = hy + 1
        tx'
            | tx < hx = hx - 1
            | otherwise = hx + 1

translate (dy, dx) (y, x) = (y + dy, x + dx)

-- Apply motion to rope; returns the "trail".
stepN :: String -> Rope -> [Rope]
stepN ('U':' ':(read -> n)) = take n . tail . steps (-1, 0)
stepN ('D':' ':(read -> n)) = take n . tail . steps (1, 0)
stepN ('L':' ':(read -> n)) = take n . tail . steps (0, -1)
stepN ('R':' ':(read -> n)) = take n . tail . steps (0, 1)

-- Repeatedly move the rope in one direction
steps :: (Int, Int) -> Rope -> [Rope]
steps dir = iterate (\(x:xs') -> scanl adjust (translate dir x) xs')

-- Apply all motions to the rope; returns the final rope and the
-- the whole trail.
simulate :: Rope -> [String] -> (Rope, [Rope])
simulate rope = second concat . mapAccumL (\xs motion -> let xs' = stepN motion xs in (last xs', xs')) rope


main = do
    input <- readFile "input.txt"
    
    -- Part 1
    let rope1 = [(0, 0), (0, 0)]
        (_, trail1) = simulate rope1 (lines input)
        tails1 = map last trail1
    print $ length $ Set.fromList tails1

    -- Part 2
    let rope2 = replicate 10 (0, 0)
        (_, trail2) = simulate rope2 (lines input)
        tails2 = map last trail2
    print $ length $ Set.fromList tails2
