{-# LANGUAGE ViewPatterns #-}
import Control.Monad
import Text.Printf

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
