import Control.Applicative
import Data.Array (inRange)
import Data.Char
import Data.Function
import Data.List
import Data.Ord
import Text.ParserCombinators.ReadP


data Point = Point
    { position :: (Int, Int)
    , velocity :: (Int, Int)
    } deriving (Eq, Ord)

parseNumber :: ReadP Int
parseNumber = negative +++ positive
  where
    negative = negate <$> (char '-' *> positive)
    positive = read <$> munch1 isDigit

parseVector :: ReadP (Int, Int)
parseVector = between (char '<') (char '>') (liftA2 (,) (skipSpaces *> parseNumber) (char ',' *> skipSpaces *> parseNumber))

parsePoint :: ReadP Point
parsePoint = do
    _ <- string "position="
    p <- parseVector
    _ <- char ' '
    _ <- string "velocity="
    v <- parseVector
    return $ Point p v

parse = sepBy parsePoint (char '\n') <* char '\n' <* eof

step = fmap $ \(Point (x, y) (dx, dy)) -> Point (x + dx, y + dy) (dx, dy)

bounds = foldl1 mergeBounds . fmap (\p -> (position p, position p))

mergeBounds ((x1, y1), (x2, y2)) ((x3, y3), (x4, y4)) = ((min x1 x3, min y1 y3), (max x2 x4, max y2 y4)) 

showPoints ((x1, y1), (x2, y2)) ps = unlines [[maybe '.' id (lookup (x, y) (zip (map position ps) (repeat '#'))) | x <- [x1..x2]] | y <- [y1..y2]]

-- Assume that the message appears when the distance between all pairs of
-- points is no longer decreasing
findMessage = f 0 . map (\x -> (x, sumdist x)) . iterate step
    where f t ((x, dx):(y, dy):ys) = if dx < dy then (t, x) else f (t + 1) ((y, dy):ys)

sumdist ps = sum [abs (x1 - x2) + abs (y1 - y2) | (x1, y1) <- fmap position ps, (x2, y2) <- fmap position ps]

main = do
    input <- readFile "input.txt"
    let [(points, "")] = readP_to_S parse input

    -- Part 1
    -- This takes (almost) an eternity
    let (t, message) = findMessage points

    -- Assume that the message is somewhere in the center...
    let points' = filter (inRange ((-250, -250), (250, 250)) . position) message
        bounds' = bounds points'
    putStrLn $ showPoints bounds' message

    -- Part 2
    print t
