import Control.Applicative
import Data.Array
import Data.Char
import Data.List
import Data.Ord
import Text.ParserCombinators.ReadP

type Position = (Int, Int, Int)

data Nanobot = Nanobot
    { position :: Position
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

parse = endBy parseNanobot (char '\n') <* eof

inRange :: Nanobot -> Position -> Bool
inRange n p = distance (position n) p < radius n

distance (x1, y1, z1) (x2, y2, z2) = abs (x1 - x2) + abs (y1 - y2) + abs (z1 - z2)

overlaps (Nanobot {position = p1, radius = r1}) (Nanobot {position = p2, radius = r2}) =
        distance p1 p2 <= r1 + r2

bounds nanobots = ((xmin, xmax), (ymin, ymax), (zmin, zmax))
  where
    xmin = minimum (map (\Nanobot{position=(x, y, z), radius=r} -> x - r) nanobots)
    xmax = maximum (map (\Nanobot{position=(x, y, z), radius=r} -> x + r) nanobots)
    ymin = minimum (map (\Nanobot{position=(x, y, z), radius=r} -> y - r) nanobots)
    ymax = maximum (map (\Nanobot{position=(x, y, z), radius=r} -> y + r) nanobots)
    zmin = minimum (map (\Nanobot{position=(x, y, z), radius=r} -> z - r) nanobots)
    zmax = maximum (map (\Nanobot{position=(x, y, z), radius=r} -> z + r) nanobots)

search :: [Nanobot] -> (Int, Int, Int)
search nanobots = go Nanobot{position=centroid, radius=searchRadius}
  where
    ((xmin, xmax), (ymin, ymax), (zmin, zmax)) = Main.bounds nanobots
    centroid = ((xmax + xmin) `div` 2, (ymax + ymin) `div` 2, (zmax + zmin) `div` 2)
    searchRadius = maximum [xmax - xmin, ymax - ymin, zmax - zmin] `div` 2

    overlapping n = length (filter (overlaps n) nanobots)

    subspaces (Nanobot {position = (x, y, z), radius = r}) =
        [ Nanobot {position=(x - dr, y, z), radius=r'}
        , Nanobot {position=(x + dr, y, z), radius=r'}
        , Nanobot {position=(x, y - dr, z), radius=r'}
        , Nanobot {position=(x, y + dr, z), radius=r'}
        , Nanobot {position=(x, y, z - dr), radius=r'}
        , Nanobot {position=(x, y, z + dr), radius=r'}
        , Nanobot {position=(x, y, z), radius=r'}
        ]
      where 
          r' = min ((r * 2 + 2) `div` 3) (r - 1)
          dr = r - r'

    go b@Nanobot{position=p, radius=1}         = p
    go b@Nanobot{position=(x, y, z), radius=r} = go b'
      where
        qs = subspaces b
        b' = maximumBy (comparing overlapping) qs

main = do
    input <- readFile "input.txt"
    let [(nanobots, _)] = readP_to_S parse input

    -- Part 1
    let strongest = maximumBy (comparing radius) nanobots
    print $ length $ filter (Main.inRange strongest . position) nanobots

    -- Part 2
    let (x, y, z) = search nanobots
    print $ distance (x, y, z) (0, 0, 0)
