{-# LANGUAGE ViewPatterns #-}
import Data.List (sort)

type Interval = (Integer, Integer)

parse = map parseLine . lines
parseLine (break (== '-') -> (x, '-':y)) = (read x, read y)

merge []  = []
merge [x] = [x]
merge ((lo0, hi0):(lo1, hi1):xs)
    | lo1 - 1 <= hi0 = merge ((min lo0 lo1, max hi0 hi1):xs)
    | otherwise = (lo0, hi0):merge ((lo1, hi1):xs)

converge :: (a -> a -> Bool) -> [a] -> a
converge p (x:ys@(y:_))
    | p x y     = y
    | otherwise = converge p ys

minimize = converge (==) . (iterate merge) . sort

main1 = readFile "input.txt" >>= print . succ . snd . head . minimize . parse

--- Part Two ---
main2 = readFile "input.txt" >>= print . sum . map pred . map (uncurry (flip (-))) . invert (0, 4294967295) . minimize . parse

invert :: Interval -> [Interval] -> [Interval]
invert (lo, hi) = invert'
  where
    invert' [(lo0, hi0)]
        | hi0 >= hi = []
        | otherwise = [(hi0, hi)]
    invert' ((lo0, hi0):x@(lo1, hi1):xs)
        | lo0 > lo  = (lo, lo0) : invert (lo0, hi) (x:xs)
        | otherwise = (hi0, lo1) : invert (lo1, hi) (x:xs)

main = main1 >> main2
