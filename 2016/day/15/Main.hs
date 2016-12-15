{-# LANGUAGE ViewPatterns #-}
import Control.Monad
import Text.Printf
import Data.Maybe
-- import ChineseRemainder
import CRT

--- Day 15: Timing is Everything ---

type Disc = Int -> Int

parse :: String -> [Disc]
parse = map parseLine . lines

parseLine :: String -> Disc
parseLine (words -> ["Disc",disc,"has",read -> n,"positions;","at","time=0,","it","is","at","position",break (== '.') -> (read -> p, ".")]) =
    \i -> (p + i) `mod` n

testDiscs = [\i -> (4 + i) `mod` 5, \i -> (1 + i) `mod` 2]

solutions discs = do
    t <- [0..]
    let xs = map (\(disc, dt) -> disc (t + dt)) (zip discs [1..])
    guard (all (== 0) xs)
    return t

solution = head . solutions

test = solution testDiscs

main1 = readFile "input.txt" >>= print . solution . parse

--- Part Two ---

input2 = "Disc #X has 11 positions; at time=0, it is at position 0."

main2 = readFile "input.txt" >>= print . solution . (++ [parseLine input2]) . parse

main = main1 >> main2

-- While easy to solve using brute force, a more beautiful way
-- is to use the chinese remainder theorem. Luckily, I found some
-- excellent code for solving CRT online, here:
--
--   http://rosettacode.org/wiki/Chinese_remainder_theorem#Haskell
--
-- and (honorable mention), here:
--
--   https://gist.github.com/kgadek/5503271
parse3 = map parseLine3 . lines

parseLine3 :: String -> (Integer, Integer)
parseLine3 (words -> ["Disc",disc,"has",read -> n,"positions;","at","time=0,","it","is","at","position",break (== '.') -> (read -> p, ".")]) = (p, n)

main3 = do
    input <- readFile "input.txt"
    let (residuals, modulii) = unzip . parse3 $ input
        residuals' = zipWith (+) residuals [1..]  -- Add the time steps
        Just n = chineseRemainder residuals' modulii
        solution = abs (n - product modulii)
    print solution
