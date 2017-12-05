{-# LANGUAGE RecordWildCards, ViewPatterns #-}
module Main where

import Data.Array
import Data.List

data Cpu =
    Cpu { pc :: Int,
          mem :: Array Int Int
        }
  deriving (Show)

parse :: String -> Cpu
parse s =
    let memArr = map read $ lines s
        mem = listArray (0, length memArr - 1) memArr
        pc = 0
    in Cpu pc mem

overflow Cpu{..} = not $ inRange (bounds mem) pc

step1 :: Cpu -> Cpu
step1 Cpu{..} =
      let v = mem ! pc
          pc' = pc + v
          mem' = mem // [(pc, v + 1)]
          cpu' = Cpu pc' mem'
      in cpu'

step2 :: Cpu -> Cpu
step2 Cpu{..} =
      let v = mem ! pc
          pc' = pc + v
          mem' = mem // [(pc, if v >= 3 then v - 1 else v + 1)]
          cpu' = Cpu pc' mem'
      in cpu'

run step = length . takeWhile (not . overflow) . iterate step

test = run step2 $ Cpu 0 $ listArray (0, 4) (map read (words "0 3 0 1 -3"))

main = do
    input <- readFile "input.txt"
    print . run step1 . parse $ input
    print . run step2 . parse $ input
