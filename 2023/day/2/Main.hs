import Data.Char (isDigit)
import Data.Function (on)
import Data.Functor
import Data.List
import Data.Maybe
import Data.Ord
import Text.ParserCombinators.ReadP

data Color = Red | Green | Blue deriving (Eq, Ord, Show)

newtype Round = Round [(Color, Int)] deriving (Show)

data Game = Game {gameId :: Int, rounds :: [Round]} deriving (Show)

pcolor :: ReadP Color
pcolor = choice [string "red" $> Red, string "green" $> Green, string "blue" $> Blue]

pcube :: ReadP (Color, Int)
pcube = flip (,) <$> pnum <*> (char ' ' *> pcolor)

pnum :: ReadP Int
pnum = read <$> munch1 isDigit

pround :: ReadP Round
pround = Round <$> sepBy1 pcube (string ", ")

pgame :: ReadP Game
pgame = Game <$> (string "Game " *> pnum <* string ": ") <*> sepBy1 pround (string "; ")

valid1 :: Game -> Bool
valid1 = all p . rounds
  where
    p (Round xs) = fromMaybe 0 (lookup Red xs) <= 12 && fromMaybe 0 (lookup Green xs) <= 13 && fromMaybe 0 (lookup Blue xs) <= 14

fewest :: Game -> [(Color, Int)]
fewest (Game _ rounds) = map (\((color, n) : cubes) -> (color, maximum (n : map snd cubes))) $ groupBy ((==) `on` fst) $ sortBy (comparing fst) $ concatMap (\(Round xs) -> xs) rounds

power :: [(Color, Int)] -> Int
power = product . map snd

main = do
  input <- readFile "input"
  let [(games, "")] = readP_to_S (endBy1 pgame skipSpaces <* eof) input

  print $ sum $ map gameId $ filter valid1 games
  print $ sum $ map (power . fewest) games