{-# LANGUAGE ViewPatterns #-}
import Data.Char (isDigit)
import qualified Data.IntMap as Map
import Data.Sequence
import Text.ParserCombinators.ReadP


rotatel 0 as                 = as
rotatel n (viewl -> a :< as) = rotatel (n - 1) (as |> a)

rotater 0 as                 = as
rotater n (viewr -> as :> a) = rotater (n - 1) (a <| as)

-- Play the Elves favourite marble game
marble :: Int  -- Number of players
       -> Int  -- Value of last marble
       -> [(Int, Int)]  -- List of all points received as (player, points)
marble n m = go (cycle [1..n]) [1..m] (singleton 0)
  where
    go _      []     xs = []
    go (p:ps) (m:ms) xs
        | m `mod` 23 == 0 =
            let (x :< xs') = viewl $ rotater 7 xs
            in (p, x + m) : go ps ms xs'
        | otherwise =
            let (x :< xs') = viewl $ rotatel 1 xs
            in go ps ms ((m <| xs') |> x)

-- Sum each players' score
score = Map.assocs . Map.fromListWith (+)

highscore = maximum . map snd . score

parse = do
    p <- parseNumber
    _ <- string " players; last marble is worth "
    m <- parseNumber
    _ <- string " points"
    return (p, m)
  where

parseNumber = read <$> many1 (satisfy isDigit)

parseTest = do
    (p, m) <- parse
    s <- string ": high score is " *> parseNumber
    return (p, m, s)

main = do
    input <- readFile "input.txt"
    let [((p, m), "")] = readP_to_S (parse <* char '\n' <* eof) input

    -- Part 1
    print $ highscore $ marble p m

    -- Part 2
    print $ highscore $ marble p (m * 100)
