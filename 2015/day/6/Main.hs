{-# LANGUAGE TupleSections, ViewPatterns #-}
import Data.Array
import Data.Char
import Text.ParserCombinators.ReadP

type Grid = Array (Int, Int) Int

data Phrase = TurnOn | TurnOff | Toggle

parseCoord :: ReadP (Int, Int)
parseCoord = do
    x <- read <$> many1 (satisfy isDigit)
    _ <- char ','
    y <- read <$> many1 (satisfy isDigit)
    eof
    return (x, y)

coord :: String -> Maybe (Int, Int)
coord (readP_to_S parseCoord -> [(c, "")]) = Just c
-- coord (break (== ',') -> (read -> i, ',':(read -> j))) = Just (i, j)
coord _ = Nothing

step :: (Phrase -> Int -> Int)
     -> Grid
     -> String
     -> Grid
step f a (words -> ["turn", "on", coord -> Just x, "through", coord -> Just y]) =
    a // map (\ix -> (ix, f TurnOn (a ! ix))) (range (x, y))
step f a (words -> ["turn", "off", coord -> Just x, "through", coord -> Just y]) =
    a // map (\ix -> (ix, f TurnOff (a ! ix))) (range (x, y))
step f a (words -> ["toggle", coord -> Just x, "through", coord -> Just y]) =
    a // map (\ix -> (ix, f Toggle (a ! ix))) (range (x, y))

main = do
    input <- readFile "input.txt"
    let grid = listArray ((0, 0), (999, 999)) (repeat 0)

    -- Part 1
    let f1 TurnOn _ = 1
        f1 TurnOff _ = 0
        f1 Toggle 0 = 1
        f1 Toggle 1 = 0
    print $ sum $ elems $ foldl (step f1) grid (lines input)

    -- Part 2
    let f2 TurnOn e = e + 1
        f2 TurnOff e = max 0 (e - 1)
        f2 Toggle e = e + 2
    print $ sum $ elems $ foldl (step f2) grid (lines input)
