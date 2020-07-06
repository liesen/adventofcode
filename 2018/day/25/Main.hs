{-# LANGUAGE ViewPatterns #-}
import Data.Char
import Data.List
import Text.ParserCombinators.ReadP


type Point = [Int]

parse :: ReadP [Point]
parse = many p
    where
        p :: ReadP Point
        p = do
            skipSpaces
            xs <- count 3 (read <$> munch1 (/= ',') <* char ',')
            y <- read <$> munch1 (/= '\n')
            char '\n'
            return (xs ++ [y])

dist :: Point -> Point -> Int
dist a b = sum $ map abs $ zipWith (-) a b

cluster :: [Point] -> [[Point]]
cluster xs = go xs []
    where
        go :: [Point] -> [[Point]] -> [[Point]]
        go []     cs = cs
        go (z:zs) (partition (any ((<= 3) . dist z)) -> (xs, ys)) = go zs ((z:concat xs):ys)

main = do
    input <- readFile "input.txt"
    let points = head [x | (x, "") <- readP_to_S (parse <* eof) input]

    -- Part 1
    let constellations = cluster points
    print $ length constellations