import Data.List
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Text.ParserCombinators.ReadP

-- Regex data type
data Re
    = Eps         -- Empty option
    | Str String  -- Route
    | Alt [Re]    -- Branch
    | Seq [Re]
  deriving (Eq, Show)

-- Parsing stuff
parse = between (char '^') (char '$') parseRe <* optional (char '\n') <* eof

parseStr = Str <$> munch1 (`elem` "NWSE")

parseAlt = Alt <$> between (char '(') (char ')') (sepBy1 parseRe (char '|'))

parseRe = do
    xs <- many (parseAlt +++ parseStr)
    case xs of
      [] -> return Eps
      [x] -> return x
      xs -> return (Seq xs)

-- Map/graph stuff
type Coord = (Int, Int)
type Dist = Int

walk = snd . walkRe

-- Walk every combination of the regex and find distances to every room
walkRe :: Re -> ((Coord, Dist), [(Coord, Dist)])
walkRe = go ((0, 0), 0)
  where
    go p Eps      = (p, [])
    go p (Str s)  = mapAccumL (\p' c -> let y = step p' c in (y, y)) p s
    go p (Alt xs) = foldl' (\(p, ys) x -> let (p', ys') = go p x in (p, ys <> ys')) (p, []) xs
    go p (Seq xs) = foldl' (\(p, ys) x -> let (p', ys') = go p x in (p', ys <> ys')) (p, []) xs

step :: (Coord, Dist) -> Char -> (Coord, Dist)
step ((x, y), n) 'E' = ((x + 1, y), n + 1)
step ((x, y), n) 'W' = ((x - 1, y), n + 1)
step ((x, y), n) 'N' = ((x, y - 1), n + 1)
step ((x, y), n) 'S' = ((x, y + 1), n + 1)

main = do
  input <- readFile "input.txt"
  let [(re, "")] = readP_to_S parse input

  -- Path lengths to all rooms from (0, 0)
  let ps = walk re
  
  -- Shortest path to all rooms
  let sps = Map.fromListWith min ps
      distances = map snd (Map.toList sps)

  -- Part 1
  print $ maximum distances

  -- Part 2
  print $ length $ filter (>= 1000) distances
