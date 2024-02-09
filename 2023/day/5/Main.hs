import Control.Monad
import Data.Char
import Data.Maybe
import Data.Monoid
import Data.Traversable
import Debug.Trace
import Text.ParserCombinators.ReadP
import Data.List (minimum)

data Range = Range Int Int deriving (Eq, Ord, Show)

data Trans = Trans Int Range deriving (Eq, Show)

data Map = Map String [Trans] deriving (Eq, Show)

singleton n = Range n 1

{-
          a1          a2
          |           |
b1-----b2 |           |
          |           |
      b1-----b2       |
          |           |
      b1-----------------b2
          |           |
          | b1-----b2 |
          |           |
          |       b1-----b2
          |           |
          |           | b1-----b2
 -}
overlap (Range a1 n) b@(Range b1 m)
  | b2 < a1 = (Nothing, [b])
  | b1 <= a2 = (Just (Range (max a1 b1) (min a2 b2 - max a1 b1)), filter nonEmpty [Range (min b1 a1) (a1 - min b1 a1), Range a2 (b2 - a2)])
  | a2 < b1 = (Nothing, [b])
  where
    a2 = a1 + n
    b2 = b1 + m

    nonEmpty (Range x n) = n > 0

newline = char '\n'

parse :: ReadP ([Int], [Map])
parse = do
  seeds <- string "seeds: " *> sepBy1 (read <$> munch1 isDigit) skipSpaces <* newline <* skipSpaces
  maps <- sepBy1 parseMap newline
  return (seeds, maps)

parseMap =
  skipSpaces *> do
    name <- many1 (satisfy (not . isSpace)) <* string " map:" <* newline
    transformations <-
      endBy
        ( Trans
            <$> number
            <*> (Range <$> (char ' ' *> number) <*> (char ' ' *> number))
        )
        newline
    return $ Map name transformations

ranges [] = []
ranges (x : y : ys) = Range x y : ranges ys

number :: ReadP Int
number = read <$> munch1 isDigit

run :: [Range] -> Map -> [Range]
run as (Map _ tfs) = let (as', bss) = mapAccumL step as tfs in as' ++ concat bss

step as t = let (as', bs) = unzip (map (`trans` t) as) in (concat as', concat bs)

trans a (Trans dest (Range start n)) =
    case Range start n `overlap` a of
      (Just (Range x m), as) ->
        let s' = Range (dest + x - start) m
        in (as, [s'])
      (Nothing, as) -> (as, [])

main = do
  input <- readFile "input"
  let [((seeds, maps), "")] = readP_to_S (parse <* skipSpaces <* eof) input
  
  -- Part 1
  print $ minimum [n | Range n _ <- foldl run (map singleton seeds) maps]

  -- Part 2
  print $ minimum [n | Range n _ <- foldl run (ranges seeds) maps]