{-# LANGUAGE MultilineStrings #-}

import Control.Monad
import Data.Char
import Data.Function (on)
import Data.Ix
import Data.List
import Data.Maybe
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Traversable
import Debug.Trace
import Text.ParserCombinators.ReadP

data Shape = Shape Int (Int, Int) (Set (Int, Int)) -- deriving (Show)

instance Show Shape where
  show (Shape i (rmax, cmax) points) =
    unlines [[if (r, c) `elem` points then '#' else '.' | c <- [0 .. cmax]] | r <- [0 .. rmax]]

data Constraint = Constraint (Int, Int) [(Int, Int)] deriving (Show)

data Input = Input [Shape] [Constraint] deriving (Show)

number :: ReadP Int
number = read <$> munch1 isDigit

newline, space :: ReadP Char
newline = char '\n'
space = char ' '

parseShape :: ReadP Shape
parseShape = do
  ident <- number <* char ':' <* newline
  shapeLines <- many1 (satisfy (`elem` ".#")) `endBy1` newline
  let points = [(r, c) | (r, line) <- zip [0 ..] shapeLines, (c, ch) <- zip [0 ..] line, ch == '#']
      rmax = maximum [r | (r, c) <- points]
      cmax = maximum [c | (r, c) <- points]
      bounds = (rmax, cmax)
  pure $ Shape ident bounds (Set.fromList points)

parse = Input <$> parseShapes <*> parseConstraints
  where
    parseShapes = parseShape `endBy1` newline
    parseRegionSize = do
      x <- number
      y <- char 'x' *> number
      pure (y - 1, x - 1)
    parseConstraint = Constraint <$> parseRegionSize <*> (string ": " *> parseX)
    parseX = zip [0 ..] <$> number `sepBy1` space
    parseConstraints = parseConstraint `sepBy1` newline

-- Plan: while there are shapes to place
-- pick next shape (id) in list
-- try to place any of the variants at any of the free spaces (don't
-- care if it "fits" bbox wise) using set intersection
-- yes? place next recursively
-- no? fail (this shape must be placeable)
--- place shapes constraints
transformations :: Shape -> [Shape]
transformations (Shape i (rmax, cmax) ps) =
  [identity, ccw90, ccw180, ccw270, flipH, flipV, flipDiagA, flipDiagB]
  where
    identity = Shape i (rmax, cmax) ps
    ccw90 = Shape i (cmax, rmax) $ Set.map (\(r, c) -> (c, rmax - r)) ps
    ccw180 = Shape i (rmax, cmax) $ Set.map (\(r, c) -> (rmax - r, cmax - c)) ps
    ccw270 = Shape i (cmax, rmax) $ Set.map (\(r, c) -> (cmax - c, r)) ps
    flipH = Shape i (rmax, cmax) $ Set.map (\(r, c) -> (r, cmax - c)) ps
    flipV = Shape i (rmax, cmax) $ Set.map (\(r, c) -> (rmax - r, c)) ps
    flipDiagA = Shape i (cmax, rmax) $ Set.map (\(r, c) -> (c, r)) ps
    flipDiagB = Shape i (cmax, rmax) $ Set.map (\(r, c) -> (cmax - c, rmax - r)) ps

-- translate (dr, dc) (Shape i bnds ps) = Shape i bnds [(r + dr, c + dc) | (r, c) <- ps]

{-
place shapes (Constraint (rmax, cmax) []) occupied = True -- return occupied
place shapes (Constraint size ((i, 0) : constraints)) occupied = place shapes (Constraint size constraints) occupied
place shapes (Constraint (rmax, cmax) ((i, n) : constraints)) occupied = or $ do
  let Just shape = find (\(Shape i' _ _) -> i' == i) shapes
  Shape i (rmax', cmax') ps <- transformations shape
  -- Check if we can translate and place shape' at any open location of
  -- (r, c) <- range ((0, 0), (rmax, cmax)) \\ map fst occupied
  -- No need to place it in the void, place it next to a current shape or at (0, 0)
  -- (r, c) <- (0, 0) : concatMap (\(r, c) -> [(r, c + 1), (r + 1, c), (r + 1, c + 1)]) occupied
  (r, c) <- (0, 0) : concatMap (\(r, c) -> [(r + dr, c + dc) | dr <- [-1, 0, 1], dc <- [-1, 0, 1]]) occupied
  guard $ r >= 0 && r <= rmax && c >= 0 && c <= cmax
  -- guard $ (r, c) `Set.notMember` occupied
  guard $ r + rmax' <= rmax && c + cmax' <= cmax
  let ps' = Set.map (\(r', c') -> (r + r', c + c')) ps
  guard $ Set.null (Set.intersection occupied ps')
  let occupied' = Set.union occupied ps'
  return $ place shapes (Constraint (rmax, cmax) ((i, n - 1) : constraints)) occupied'
-}

place shapes (Constraint (rmax, cmax) []) occupied = True -- return occupied
place shapes (Constraint size ((i, 0) : constraints)) occupied = place shapes (Constraint size constraints) occupied
place shapes (Constraint (rmax, cmax) ((i, n) : constraints)) occupied = or $ do
  let Just shape = find (\(Shape i' _ _) -> i' == i) shapes
  Shape i (rmax', cmax') ps <- transformations shape
  -- Check if we can translate and place shape' at any open location of
  -- No need to place it in the void, place it next to a current shape or at (0, 0)
  (r, c) <- (0, 0) : concatMap (\(r, c) -> [(r, c + 1), (r + 1, c), (r + 1, c + 1)]) occupied
  guard $ (r, c) `Set.notMember` occupied
  let fits = r + rmax' <= rmax && c + cmax' <= cmax
  guard fits
  let ps' = Set.map (\(r', c') -> (r + r', c + c')) ps
  -- let overlaps = any (`elem` map fst occupied) ps'
  let notOverlaps = Set.null (Set.intersection occupied ps')
  guard notOverlaps
  -- let occupied' = unionBy ((==) `on` fst) [(p, letter i) | p <- Set.elems ps'] occupied
  let occupied' = Set.union occupied ps'
  return $ place shapes (Constraint (rmax, cmax) ((i, n - 1) : constraints)) occupied'

letter i = chr (ord 'A' + i)

showPlacement (Constraint (rmax, cmax) _) placement =
  unlines
    -- [ [maybe '.' id (lookup (r, c) placement) | c <- [0 .. cmax]]
    [ [if Set.member (r, c) placement then '#' else '.' | c <- [0 .. cmax]]
    | r <- [0 .. rmax]
    ]

main = do
  let [(problem@(Input shapes constraints), "")] = readP_to_S (parse <* eof) input
      Just shape0 = listToMaybe [sh | sh@(Shape i _ _) <- shapes, i == 0]
  mapM_ print (transformations shape0)
  putStrLn "--------"
  -- print $ length $ filter (\constraint -> place shapes constraint mempty) constraints
  forM_ constraints $ \constraint -> do
    print constraint
    let placable = place shapes constraint mempty
    print placable

input =
  """
  0:
  ###
  ##.
  ##.

  1:
  ###
  ##.
  .##

  2:
  .##
  ###
  ##.

  3:
  ##.
  ###
  ##.

  4:
  ###
  #..
  ###

  5:
  ###
  .#.
  ###

  4x4: 0 0 0 0 2 0
  12x5: 1 0 1 0 2 2
  12x5: 1 0 1 0 3 2
  """

{-
flipH (Shape i bnds@(_, (rmax, cmax)) ps) = Shape i bnds [(rmax - r, c) | (r, c) <- ps]

flipV (Shape i bnds@(_, (rmax, cmax)) ps) = Shape i bnds [(r, cmax - c) | (r, c) <- ps]

rotL (Shape i bnds@(_, (rmax, cmax)) ps) = Shape i bnds [(c, rmax - r) | (r, c) <- ps]
-}