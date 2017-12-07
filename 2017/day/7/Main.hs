{-# LANGUAGE ViewPatterns #-}
module Main where

import Control.Monad
import Control.Applicative
import Data.Graph
import Data.List (nub)
import Data.Maybe (mapMaybe)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Monoid
import Data.Tree

parse = map parseLine . lines

parseLine :: String -> (Int, String, [String])
parseLine (words -> [node, read -> weight]) = (weight, node, [])
parseLine (words -> (node:(readParen True reads -> [(weight, "")]):"->":xs)) = (weight, node, map (filter (/= ',')) xs)

parseGraph = graphFromEdges . parse

main = do
    input <- readFile "input.txt"
    let (graph, vertex, key) = parseGraph input
        (rootKey:_) = topSort graph
        (_, rootNode, _) = vertex rootKey
    putStrLn rootNode

    -- Part 2
    let tree = unfoldTree (\k -> let (w, x, xs) = vertex k in (k, mapMaybe key xs)) rootKey
    -- putStrLn (drawTree (fmap (show . vertex) tree))

    -- Sum weights bottom-up
    let weights = foldr (\(vertex -> (w, x, xs)) m -> Map.insert x (w + sum (mapMaybe (flip Map.lookup m) xs)) m) Map.empty tree
        balanced (vertex -> (w, x, xs)) = length (nub (mapMaybe (flip Map.lookup weights) xs)) <= 1
        weighted (vertex -> (w, x, xs)) = (Map.!) weights x
    -- writeFile "C:\\scratch\\output.txt" (drawTree (fmap (\k -> show (vertex k, balanced k, weighted k)) tree))

    -- Find the unbalanced node
    let Just unbalancedKey = find' (not . balanced) tree
        unbalancedTree = unfoldTree (\k -> let (w, x, xs) = vertex k in (k, mapMaybe key xs)) unbalancedKey
    -- putStrLn (drawTree (fmap (\k -> show (vertex k, weighted k)) unbalancedTree))

    -- One of its discs is unbalanced
    {-
    let (w, x, xs) = vertex unbalancedKey
    forM_ (mapMaybe key xs) $ \k -> do
      putStr (show (vertex k))
      putStr " = "
      print (weighted k)
    -}
    -- it's jriph!  jriph's weight is 1998 but should be 1993 because the sum
    -- of it's discs is 2102 but should be 2097
    print 1993

-- Traversable.find but get's the last element
find' p = getLast . foldMap (\x -> Last (if p x then Just x else Nothing))
