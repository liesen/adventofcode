{-# LANGUAGE BangPatterns, ImportQualifiedPost #-}
import Data.Graph
import Data.Tree
import Control.Monad
import Data.Char
import Data.List
import Data.Maybe
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Text.ParserCombinators.ReadP

parseNumber :: ReadP Int
parseNumber = (negate <$> (char '-' *> n)) +++ n
  where
    n :: ReadP Int
    n = read <$> munch1 isDigit

parseCoord = (,,) <$> parseNumber <*> (char ',' *> parseNumber) <*> (char ',' *> parseNumber)

parseHeader :: ReadP Int
parseHeader = string "--- scanner " *> n <* string " ---"
  where
    n = read <$> munch1 isDigit

parseScanner = (,) <$> (parseHeader <* char '\n') <*> (Set.fromList <$> sepBy parseCoord (char '\n'))

-- https://www.euclideanspace.com/maths/discrete/groups/categorise/finite/cube/index.htm
rotations :: [(Int, Int, Int) -> (Int, Int, Int)]
rotations =
    [ \(x, y, z) -> (-z, -x, y)
    , \(x, y, z) -> (x, -y, -z)
    , \(x, y, z) -> (-z, -y, -x)
    , \(x, y, z) -> (z, -y, x)
    , \(x, y, z) -> (-x, -z, -y)
    , \(x, y, z) -> (y, -z, -x)
    , \(x, y, z) -> (-y, -x, -z)
    , \(x, y, z) -> (y, z, x)
    , \(x, y, z) -> (-x, z, y)
    , \(x, y, z) -> (-x, y, -z)
    , \(x, y, z) -> (x, -z, y)
    , \(x, y, z) -> (-z, x, -y)
    , \(x, y, z) -> (y, x, -z)
    , \(x, y, z) -> (z, y, -x)
    , \(x, y, z) -> (-z, y, x)
    , \(x, y, z) -> (z, x, y)
    , \(x, y, z) -> (x, y, z)
    , \(x, y, z) -> (y, -x, z)
    , \(x, y, z) -> (-y, x, z)
    , \(x, y, z) -> (-x, -y, z)
    , \(x, y, z) -> (-y, z, -x)
    , \(x, y, z) -> (-y, -z, x)
    , \(x, y, z) -> (x, z, -y)
    , \(x, y, z) -> (z, -x, -y)
    ]

-- Align two point sets by trying all rotations and translations
align :: Set (Int, Int, Int) -> Set (Int, Int, Int) -> Maybe ((Int, Int, Int) -> (Int, Int, Int))
align ps qs = listToMaybe $ do
    -- Try all rotations
    rotation <- rotations
    let qs' = Set.map rotation qs
    -- Align one point and check if that makes 11 more align
    p@(px, py, pz) <- Set.toList ps
    q@(qx, qy, qz) <- Set.toList qs'
    let dx = px - qx
        dy = py - qy
        dz = pz - qz
        qs'' = Set.map (translate (dx, dy, dz)) qs'
        -- Overlapping beacons relative to scanner "ps"
        overlapping = Set.intersection ps qs''
    guard $ length overlapping >= 12
    -- Return transformation
    return (translate (dx, dy, dz) . rotation)

translate (dx, dy, dz) (x, y, z) = (x + dx, y + dy, z + dz)

merge scanners m i = go mempty i
  where
    go seen i
      | Set.member i seen = Set.empty
      | otherwise         = Set.unions (is:[Set.map f js | (j, f) <- neighbors, let js = go (Set.insert i seen) j])
      where
        Just is = lookup i scanners
        neighbors = fromMaybe [] (Map.lookup i m)

-- Find the distances to scanners that are visible from scanner i
distances m i = map manhattan $ tail $ go mempty (i, (0, 0, 0))
  where
    go seen (i, p)
      | Set.member i seen = []
      | otherwise         = p : [f p' | (j, f) <- neighbors, p' <- go (Set.insert i seen) (j, p)]
      where
        neighbors = fromMaybe [] (Map.lookup i m)

    manhattan (dx, dy, dz) = abs dx + abs dy + abs dz

main = do
    input <- readFile "input.txt"
    let [(scanners, "")] = readP_to_S (sepBy parseScanner (string "\n\n") <* skipSpaces <* eof) input

    -- Find alignments between all pairs of scanners. This takes a 90s on my machine.
    let alignments = Map.fromListWith (++) [ (i, [(j, f)]) -- Scanner j can be aligned to scanner i by applying f
                                           | (i, is) <- scanners, (j, js) <- scanners, i /= j
                                           , f <- maybeToList (align is js)
                                           ]
    -- Part 1
    -- "Merge" into the viewpoint of scanner 0
    print $ length $ merge scanners alignments 0

    -- Part 2
    print $ maximum $ concatMap (distances alignments) $ map fst scanners
