{-# LANGUAGE ViewPatterns #-}
module Main where

import Data.List
import Data.Maybe

parseLine (break (== ':') -> (read -> depth, ':':' ':(read -> range))) = (depth, range, oscillate range)

parse = map parseLine . lines

oscillate range = cycle ([0..range - 1] ++ [range - 2, range - 3..1])

step = map f
  where f (depth, range, p:ps) = (depth, range, ps)

caught t scanners = f <$> find (\(depth, range, p:ps) -> depth == t && p == 0) scanners
  where f (depth, range, _) = (depth, range)

main = do
    input <- readFile "input.txt"
    let firewall = parse input
        maxDepth = maximum $ map (\(depth, range, ps) -> depth) firewall

    -- Part 1
    let severity = sum $ map (maybe 0 (uncurry (*))) $ zipWith caught [0..maxDepth] $ iterate step firewall
    print severity

    -- Part 2
    let (delay, _) = until (\(n, firewall) -> all isNothing $ zipWith caught [0..maxDepth] $ iterate step firewall) (\(n, firewall) -> (n + 1, step firewall)) (0, firewall)
    print delay
