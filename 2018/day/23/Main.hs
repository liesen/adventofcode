import Control.Applicative
import Data.Char
import Data.List
import Data.Ord
import Text.ParserCombinators.ReadP

data Nanobot = Nanobot
    { position :: (Int, Int, Int)
    , radius :: Int
    } deriving (Eq, Show)

parseNanobot :: ReadP Nanobot
parseNanobot = do
    pos <- string "pos=" *> between (char '<') (char '>') (liftA3 (,,) num (char ',' *> num) (char ',' *> num))
    _ <- string ", "
    r <- string "r=" *> num
    return (Nanobot pos r)
  where
    num = read <$> munch1 (\x -> x == '-' || isDigit x)

parse = sepBy parseNanobot (char '\n') <* char '\n' <* eof

inRange x x' = distance x x' <= radius x

distance Nanobot{position=(x1, y1, z1)} Nanobot{position=(x2, y2, z2)} =
        abs (x1 - x2) + abs (y1 - y2) + abs (z1 - z2)

main = do
    input <- readFile "input.txt"
    let [(nanobots, "")] = readP_to_S parse input

    -- Part 1
    let strongest = maximumBy (comparing radius) nanobots
    print $ length $ filter (inRange strongest) nanobots
