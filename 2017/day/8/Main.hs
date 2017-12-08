{-# LANGUAGE ViewPatterns #-}
module Main where

import Data.List (nub, maximumBy)
import Data.Ord

import Control.Applicative

import Data.Map (Map)
import qualified Data.Map as Map

type V = Integer

condition :: String -> (V -> V -> Bool)
condition "==" = (==)
condition "!=" = (/=)
condition "<" = (<)
condition ">" = (>)
condition "<=" = (<=)
condition ">=" = (>=)

operation :: String -> (V -> V -> V)
operation "inc" = (+)
operation "dec" = (-)

type Env = Map String V

testInput = [
    "b inc 5 if a > 1",
    "a inc 1 if b < 5",
    "c dec -10 if a >= 1",
    "c inc -20 if c == 10"
  ]

test = foldl interpret Map.empty testInput

interpret :: Env -> String -> Env
interpret env (words -> [reg,(operation -> op),(read -> b),"if",(flip (Map.findWithDefault 0) env -> x),(condition -> cond),(read -> y)])
    | x `cond` y = Map.alter (Just . maybe (0 `op` b) (`op` b)) reg env
    | otherwise  = env

testDecNeg = Map.lookup "ytr" (interpret (Map.fromList [("ytr", 0), ("xzn", 0)]) "ytr dec -258 if xzn < 9") == Just 258

main = do 
    input <- readFile "input.txt"

    -- Part 1
    print $ snd $ maximumBy (comparing snd) $ Map.assocs $ foldl interpret Map.empty (lines input)

    -- Part 2
    print $ snd $ maximumBy (comparing snd) $ Map.assocs $ Map.unionsWith max $ scanl interpret Map.empty (lines input)
