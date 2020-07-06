import Data.List
import Text.ParserCombinators.ReadP


type Point = [Int]

parse :: ReadP [Point]
parse = many $ do
            skipSpaces
            xs <- count 3 (read <$> munch1 (/= ',') <* char ',')
            y <- read <$> munch1 (/= '\n')
            char '\n'
            return (xs ++ [y])

dist :: Point -> Point -> Int
dist a b = sum $ map abs $ zipWith (-) a b

cluster :: [Point] -> [[Point]]
cluster = foldr insert []
  where
    insert p cs = let (ins, outs) = partition (any ((<= 3) . dist p)) cs
                  in (p:concat ins):outs

main = do
    input <- readFile "input.txt"
    let points = head [x | (x, "") <- readP_to_S (parse <* eof) input]

    -- Part 1
    let constellations = cluster points
    print $ length constellations