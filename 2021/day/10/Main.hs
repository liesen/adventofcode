{-# LANGUAGE LambdaCase #-}
import Text.ParserCombinators.ReadP
import Data.List

data Result
    = Ok
    | Incomplete String
    | CorruptChar Char
    deriving (Show)
        
invert = \case
  '(' -> ')'
  ')' -> '('
  '[' -> ']'
  ']' -> '['
  '{' -> '}'
  '}' -> '{'
  '<' -> '>'
  '>' -> '<'

open = flip elem "([{<"

close = flip elem ")]}>"

parse = p []

p []       []     = Ok
p []       (y:ys)
    | close y = CorruptChar y
    | otherwise = p [y] ys
p (x:xs)   []     = Incomplete (x:xs)
p (x:xs)   (y:ys)
    | open y = p (y:x:xs) ys
    | close y && open x && y == invert x = p xs ys
    | otherwise =
        -- Expected (invert x) but found y instead
        CorruptChar y

score (CorruptChar ')') = 3
score (CorruptChar ']') = 57
score (CorruptChar '}') = 1197
score (CorruptChar '>') = 25137
score _ = 0

main = do
     input <- readFile "input.txt"

     -- Part 1
     print $ sum $ map (score . parse) (lines input)
