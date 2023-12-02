import Data.Char (isDigit)
import Data.Functor
import Data.List
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Ord
import Text.ParserCombinators.ReadP

data Color = Red | Green | Blue deriving (Eq, Ord, Show)

type Cubes = Map Color Int

newtype Round = Round {cubes :: Cubes} deriving (Show)

data Game = Game {gameId :: Int, rounds :: [Round]} deriving (Show)

pcolor :: ReadP Color
pcolor = choice [string "red" $> Red, string "green" $> Green, string "blue" $> Blue]

pcube :: ReadP (Color, Int)
pcube = flip (,) <$> pnum <*> (char ' ' *> pcolor)

pnum :: ReadP Int
pnum = read <$> munch1 isDigit

pround :: ReadP Round
pround = Round . Map.fromListWith (+) <$> sepBy1 pcube (string ", ")

pgame :: ReadP Game
pgame = Game <$> (string "Game " *> pnum <* string ": ") <*> sepBy1 pround (string "; ")

-- Fewest number of cubes of each color that could have been in the bag to make the game possible
fewestPossibleCubes :: Game -> Cubes
fewestPossibleCubes = Map.unionsWith max . map cubes . rounds

possible game = Map.isSubmapOfBy (<=) (fewestPossibleCubes game) minimalSet

minimalSet :: Cubes
minimalSet = Map.fromList [(Red, 12), (Green, 13), (Blue, 14)]

power :: Cubes -> Int
power = product

main = do
  input <- readFile "input"
  let [(games, "")] = readP_to_S (endBy1 pgame skipSpaces <* eof) input

  print $ sum $ map gameId $ filter possible games
  print $ sum $ map (power . fewestPossibleCubes) games
