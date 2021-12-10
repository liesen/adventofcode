{-# LANGUAGE LambdaCase #-}
import Data.List

data Result
    = Ok
    | Incomplete String
    | Corrupt Char
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

parse = p []
    where
        p []       []     = Ok
        p []       (y:ys)
            | open y = p [y] ys
            | otherwise = Corrupt y
        p (x:xs)   []     = Incomplete (x:xs)
        p (x:xs)   (y:ys)
            | open y = p (y:x:xs) ys
            | y == invert x = p xs ys
            -- Expected (invert x) but found y instead
            | otherwise = Corrupt y

score1 (Corrupt ')') = 3
score1 (Corrupt ']') = 57
score1 (Corrupt '}') = 1197
score1 (Corrupt '>') = 25137
score1 _ = 0

score2 (Incomplete rest) = foldl' f 0 rest
  where
    f acc '(' = acc * 5 + 1
    f acc '[' = acc * 5 + 2
    f acc '{' = acc * 5 + 3
    f acc '<' = acc * 5 + 4
score2 _ = 0

main = do
     input <- readFile "input.txt"

     -- Part 1
     print $ sum $ map (score1 . parse) (lines input)

     -- Part 2
     let xs = [score2 r | r@(Incomplete _) <- parse <$> lines input]
         n = length xs
     print $ head $ drop (n `div` 2) $ sort xs
