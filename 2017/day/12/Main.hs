module Main where

-- import Text.Parsec
import Data.Graph
import Data.Maybe
import Data.Char

import Text.ParserCombinators.ReadP

parseLine = do 
    v <- node
    _ <- string " <-> "
    vs <- sepBy1 node (string ", ")
    return (-1, v, vs)
  where
    node :: ReadP Int
    node = read <$> munch1 isDigit

parse = fmap fst . listToMaybe . readP_to_S (endBy parseLine skipSpaces <* eof)

parseGraph = fmap graphFromEdges . parse

testInput = unlines [
    "0 <-> 2",
    "1 <-> 1",
    "2 <-> 0, 3, 4",
    "3 <-> 2, 4",
    "4 <-> 2, 3, 6",
    "5 <-> 6",
    "6 <-> 4, 5"
  ]

main = do
    input <- readFile "input.txt"
    let Just (graph, vertex, key) = parseGraph input

    -- Part 1
    let Just v0 = key 0
    print $ length $ reachable graph v0

    -- Part 2
    print $ length $ components graph

