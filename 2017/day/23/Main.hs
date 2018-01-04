{-# LANGUAGE ViewPatterns #-}
module Main where

import Control.Monad
import Data.Array
import Data.Char
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Numbers.Primes
import Text.Printf

data Cpu =
    Cpu { _pc :: Int
        , _mem :: Map String Int
        , _code :: Array Int String
        , _mulCnt :: Int
        }
  deriving Show

v :: Cpu -> String -> Int
v cpu x | all isAlpha x = Map.findWithDefault 0 x (_mem cpu)
        | otherwise     = read x

parse (lines -> code) =
       Cpu { _pc = 0,
             _mem = Map.fromList (zip (map show ['a'..'h']) (repeat 0)), -- Map.empty,
             _code = listArray (0, length code - 1) code,
             _mulCnt = 0
           }

step cpu = interpret (code ! pc)
  where
    pc = _pc cpu
    mem = _mem cpu
    code = _code cpu
    mulCnt = _mulCnt cpu

    interpret (words -> ["set", x, v cpu -> y]) = cpu { _pc = pc + 1, _mem = Map.insert x y mem }
    interpret (words -> ["sub", x, v cpu -> y]) = cpu { _pc = pc + 1, _mem = Map.alter (Just . maybe 0 (subtract y)) x mem }
    interpret (words -> ["mul", x, v cpu -> y]) = cpu { _pc = pc + 1, _mem = Map.alter (Just . maybe 0 (* y)) x mem, _mulCnt = mulCnt + 1 }
    interpret (words -> ["jnz", v cpu -> x, v cpu -> y]) 
        | x /= 0    = cpu { _pc = pc + y }
        | otherwise = cpu { _pc = pc + 1 }

run = until p step
  where
    p cpu = not (inRange (bounds (_code cpu)) (_pc cpu))

codegen :: Cpu -> [String]
codegen cpu = header ++ body ++ footer
  where
    header = [
        "#include <stdio.h>",
        "int main() {",
        "int a, b, c, d, e, f, g, h, mulcnt;",
        "a = b = c = d = e = f = g = h = mulcnt = 0;"
      ]
    body = do
      (n, s) <- assocs (_code cpu)
      let line = n + 1
      return $ case words s of
        ["set", x, y] -> printf "__%d: %s = %s;  // %s" line x y s
        ["sub", x, y] -> printf "__%d: %s -= %s;  // %s" line x y s
        ["mul", x, y] -> printf "__%d: %s *= %s;  mulcnt += 1; // %s" line x y s
        ["jnz", x, (read :: String -> Int) -> y] -> printf "__%d: if (%s != 0) goto __%d;  // %s" line x (line + y) s
    footer = [
        printf "__%d:" (length body + 1),
        "printf(\"a=%d b=%d c=%d d=%d e=%d f=%d g=%d h=%d, mulcnt=%d\\n\", a, b, c, d, e, f, g, h, mulcnt);",
        "return 0;",
        "}"
      ]

main = do
    input <- readFile "input.txt"
    
    -- Part 1
    print $ _mulCnt $ run $ parse input

    -- Part 2
    -- Emit some C code that can more easily be manipulated:
    -- writeFile "main_asm.c" $ unlines $ codegen $ parse input

    -- The program sets f = 0, h += 1 for non-prime numbers in the
    -- range b to c by step 17
    let b = 93 * 100 + 100000
        c = b + 17000
        range = [b, b + 17..c]
    print $ length range - length (filter isPrime range)
