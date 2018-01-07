module Main where

import Data.Array
import Data.Char
import Data.Map (Map)
import qualified Data.Map as Map
import Text.ParserCombinators.ReadP

data Move = Spin Int
          | Exchange Int Int
          | Partner Char Char
  deriving Show

move s (Spin x) = ixmap (bounds s) (\i -> (i - x) `mod` length s) s
move s (Exchange a b) = s // [(a, s ! b), (b, s ! a)]
move s (Partner a b) = fmap (\x -> if x == a then b else if x == b then a else x) s

parser = sepBy (choice [s, p, x]) (char ',') <* skipSpaces <* eof
  where
    s = do _ <- char 's'
           x <- number
           return (Spin x)
    x = do _ <- char 'x'
           a <- number
           _ <- char '/'
           b <- number
           return (Exchange a b) 
    p = do _ <- char 'p'
           a <- choice (map char ['a'..'p'])
           _ <- char '/'
           b <- choice (map char ['a'..'p'])
           return (Partner a b) 
    number = read <$> munch1 isDigit

main = do
    input <- readFile "input.txt"
    let [(moves, "")] = readP_to_S parser input
        program = listArray (0, 15) ['a'..'p']
        dance program = foldl move program moves
    
    -- Part 1
    let pos = dance program
    putStrLn $ elems pos

    -- Part 2
    let memodance s m = case Map.lookup s m of
                           Nothing -> let s' = dance s in (s', Map.insert s s' m)
                           Just s' -> (s', m)
        (pos, m) = foldl (\(k, m) _ -> memodance k m) (program, Map.empty) [1..1000000000]
    putStrLn $ elems pos
