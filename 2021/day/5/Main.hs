import Data.Ix
import Data.Char
import Data.List
import Text.ParserCombinators.ReadP

data Point = Point Int Int
  deriving (Eq, Ord, Show)

data Segment = Segment Point Point
  deriving (Eq, Ord, Show)

parseNumber = read <$> many1 (satisfy isDigit)

parsePoint = Point <$> parseNumber <*> (char ',' *> parseNumber)

parseSegment = Segment <$> parsePoint <*> (string " -> " *> parsePoint)

points1 (Segment (Point x1 y1) (Point x2 y2))
    | x1 == x2  = [Point x1 y  | y <- [min y1 y2..max y1 y2]]
    | y1 == y2  = [Point x  y1 | x <- [min x1 x2..max x1 x2]]
    | otherwise = []

points2 (Segment p@(Point x1 y1) q@(Point x2 y2)) =
    takeWhile (/= q) (iterate (.+ d) p) ++ [q]
  where
    dx = signum (x2 - x1)
    dy = signum (y2 - y1)
    d = Point dx dy
    Point x y .+ Point dx dy = Point (x + dx) (y + dy)

main = do
    input <- readFile "input.txt"
    let [(segments, "")] = readP_to_S (sepBy parseSegment (char '\n') <* skipSpaces <* eof) input

    -- Part 1
    print $ length $ filter (>= 2) $ map length $ group $ sort $ concatMap points1 segments

    -- Part 2
    print $ length $ filter (>= 2) $ map length $ group $ sort $ concatMap points2 segments
