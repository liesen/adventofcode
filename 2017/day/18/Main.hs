{-# LANGUAGE ViewPatterns #-}
module Main where

import Data.Array
import Data.Char
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map

data Cpu = 
    Cpu { _pc :: Int
        , _mem :: Map String Int
        , _code :: Array Int String
        , _sndBuf :: [Int]
        , _sndCnt :: Int
        , _rcvBuf :: [Int]
        }
  deriving Show

v :: Cpu -> String -> Int
v cpu x | all isAlpha x = Map.findWithDefault 0 x (_mem cpu)
        | otherwise     = read x

cpu (lines -> code) =
       Cpu { _pc = 0,
             _mem = Map.empty,
             _code = listArray (0, length code - 1) code,
             _sndBuf = [],
             _sndCnt = 0,
             _rcvBuf = []
           }

testCpu = cpu (unlines code)
  where
    code = [
        "set a 1",
        "add a 2",
        "mul a a",
        "mod a 5",
        "snd a",
        "set a 0",
        "rcv a",
        "jgz a -1",
        "set a 1",
        "jgz a -2 "
      ]

step cpu = interpret (code ! pc)
  where
    pc = _pc cpu
    mem = _mem cpu
    code = _code cpu
    snd = _sndBuf cpu
    sndCnt = _sndCnt cpu

    interpret (words -> ["snd", v cpu -> x]) = cpu { _pc = pc + 1, _sndBuf = x:snd, _sndCnt = sndCnt + 1 }
    interpret (words -> ["set", x, v cpu -> y]) = cpu { _pc = pc + 1, _mem = Map.insert x y mem }
    interpret (words -> ["add", x, v cpu -> y]) = cpu { _pc = pc + 1, _mem = Map.alter (Just . maybe y (+ y)) x mem }
    interpret (words -> ["mul", x, v cpu -> y]) = cpu { _pc = pc + 1, _mem = Map.alter (Just . maybe 0 (* y)) x mem }
    interpret (words -> ["mod", x, v cpu -> y]) = cpu { _pc = pc + 1, _mem = Map.alter (Just . maybe 0 (`mod` y)) x mem }
    interpret (words -> ["rcv", flip Map.lookup mem -> Just x])
        | x == 0    = cpu { _pc = pc + 1 }
        | otherwise = cpu { _pc = pc + 1, _rcvBuf = head snd:_rcvBuf cpu }
    interpret (words -> ["jgz", v cpu -> x, v cpu -> y]) 
        | x > 0     = cpu { _pc = pc + y }
        | otherwise = cpu { _pc = pc + 1 }

-- Part 2
twinstep (cpu0, cpu1) = communicate (step cpu0, step cpu1)
  where
    step cpu = interpret (code ! pc)
      where
        pc = _pc cpu
        mem = _mem cpu
        code = _code cpu
        sndBuf = _sndBuf cpu
        sndCnt = _sndCnt cpu
        rcvBuf = _rcvBuf cpu
    
        interpret (words -> ["snd", v cpu -> x]) = cpu { _pc = pc + 1, _sndBuf = x:sndBuf, _sndCnt = sndCnt + 1 }
        interpret (words -> ["set", x, v cpu -> y]) = cpu { _pc = pc + 1, _mem = Map.insert x y mem }
        interpret (words -> ["add", x, v cpu -> y]) = cpu { _pc = pc + 1, _mem = Map.alter (Just . maybe y (+ y)) x mem }
        interpret (words -> ["mul", x, v cpu -> y]) = cpu { _pc = pc + 1, _mem = Map.alter (Just . maybe 0 (* y)) x mem }
        interpret (words -> ["mod", x, v cpu -> y]) = cpu { _pc = pc + 1, _mem = Map.alter (Just . maybe 0 (`mod` y)) x mem }
        interpret (words -> ["rcv", x]) =
            case rcvBuf of
              []   -> cpu  -- Wait for data
              y:ys -> cpu { _pc = pc + 1, _rcvBuf = ys, _mem = Map.insert x y mem }
        interpret (words -> ["jgz", v cpu -> x, v cpu -> y]) 
            | x > 0     = cpu { _pc = pc + y }
            | otherwise = cpu { _pc = pc + 1 }

-- Have both programs send and receive data
communicate (cpu0, cpu1) = (cpu0 { _rcvBuf = _rcvBuf cpu0 ++ _sndBuf cpu1, _sndBuf = [] },
                            cpu1 { _rcvBuf = _rcvBuf cpu1 ++ _sndBuf cpu0, _sndBuf = [] })

waiting cpu = case words (code ! pc) of
                "rcv":_ -> null rcv -- At rcv instruction but buffer empty
                _       -> False
  where
    pc = _pc cpu
    code = _code cpu
    rcv = _rcvBuf cpu

deadlock (cpu0, cpu1) = waiting cpu0 && waiting cpu1

main = do
    input <- readFile "input.txt"

    -- Part 1
    let cpu' = until (not . null . _rcvBuf) step (cpu input)
    print $ head (_rcvBuf cpu')

    -- Part 2
    let code = input -- unlines ["snd 1", "snd 2", "snd p", "rcv a", "rcv b", "rcv c", "rcv d"]
        cpu0 = let x = cpu code in x { _mem = Map.insert "p" 0 (_mem x) }
        cpu1 = let x = cpu code in x { _mem = Map.insert "p" 1 (_mem x) }
        (cpu0', cpu1') = until deadlock twinstep (cpu0, cpu1)
    print $ _sndCnt cpu1'
