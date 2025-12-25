{-# LANGUAGE MultilineStrings #-}

import Control.Monad
import Data.Char (isDigit)
import Data.List
import Data.Maybe
import Data.Set (Set)
import Data.Set qualified as Set
import Text.ParserCombinators.ReadP

data Shape = Shape (Int, Int) (Set (Int, Int)) -- deriving (Show)

bboxSize (Shape (ymax, xmax) _) = (ymax + 1) * (xmax + 1)

instance Show Shape where
  show (Shape (rmax, cmax) points) =
    unlines [[if (r, c) `elem` points then '#' else '.' | c <- [0 .. cmax]] | r <- [0 .. rmax]]

data Constraint = Constraint (Int, Int) [(Int, Int)] deriving (Show)

data Input = Input [(Int, Shape)] [Constraint] deriving (Show)

number :: ReadP Int
number = read <$> munch1 isDigit

newline, space :: ReadP Char
newline = char '\n'
space = char ' '

parseShape :: ReadP (Int, Shape)
parseShape = do
  shapeId <- number <* char ':' <* newline
  shapeLines <- many1 (satisfy (`elem` ".#")) `endBy1` newline
  let points = [(y, x) | (y, line) <- zip [0 ..] shapeLines, (x, ch) <- zip [0 ..] line, ch == '#']
      ymax = maximum [y | (y, x) <- points]
      xmax = maximum [x | (y, x) <- points]
      bounds = (ymax, xmax)
  pure (shapeId, Shape bounds (Set.fromList points))

parse = Input <$> shapes <*> constraints
  where
    shapes = parseShape `endBy1` newline
    regionSize = do
      x <- number
      y <- char 'x' *> number
      pure (y, x)
    presents = zip [0 ..] <$> number `sepBy1` space
    constraint = Constraint <$> regionSize <*> (string ": " *> presents)
    constraints = constraint `sepBy1` newline

impossible shapes (Constraint (ylen, xlen) presents) =
  sum
    [ n * length points
    | (shapeId, n) <- presents,
      (Shape _ points) <- maybeToList (lookup shapeId shapes)
    ]
    > regionSize
  where
    regionSize = xlen * ylen

trivial shapes (Constraint (ylen, xlen) presents) =
  sum
    [ n * bboxSize sh
    | (shapeId, n) <- presents,
      sh <- maybeToList (lookup shapeId shapes)
    ]
    <= regionSize
  where
    regionSize = xlen * ylen

main = do
  input <- getContents
  let [(problem@(Input shapes constraints), "")] = readP_to_S (parse <* skipSpaces <* eof) input

  -- Part 1: turns out all the possible regions are all trivial to pack
  let possible = filter (not . impossible shapes) constraints

  if length possible == length (filter (trivial shapes) possible)
    then print (length possible)
    else print "Too much work for Santa"

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