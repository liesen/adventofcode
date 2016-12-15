{-# LANGUAGE ViewPatterns, LambdaCase #-}
import Data.Array
import Data.Char
import Data.List

import Debug.Trace

type Instruction = String

-- Operand: a register or an immediate value
data Operand =
      R Int  -- Register
    | V Int  -- Immediate

op "a" = R 0
op "b" = R 1
op "c" = R 2
op "d" = R 3
op x   = V (read x)

data Cpu =
    Cpu { _reg :: Array Int Int,
          _pc :: Int,
          _code :: Array Int Instruction
        }

instance Show Cpu where
    show cpu = "{PC=" ++ show (_pc cpu) ++ " " ++
               unwords (map showReg (assocs (_reg cpu))) ++
               "}"
      where
        showReg (r, v) = chr (r + ord 'a'):'=':show v

step cpu =
    case words (_code cpu ! _pc cpu) of
      ["cpy", (v -> (_, x)), (op -> R r)] ->
        cpu { _reg = _reg cpu // [(r, x)], _pc = _pc cpu + 1 }
      ["inc", (v -> (R r, x))] ->
        cpu { _reg = _reg cpu // [(r, x + 1)], _pc = _pc cpu + 1 }
      ["dec", (v -> (R r, x))] ->
        cpu { _reg = _reg cpu // [(r, x - 1)], _pc = _pc cpu + 1 }
      ["jnz", (v -> (_, 0)), _] ->
        cpu { _pc = _pc cpu + 1 }
      ["jnz", (v -> _), (op -> V y)] ->
        cpu { _pc = _pc cpu + y }
  where
    v (op -> V x) = (V x, x)
    v (op -> R r) = (R r, _reg cpu ! r)

cpu0 xs = Cpu { _reg = listArray (0, 3) [0, 0, 0, 0],
                _pc = 0,
                _code = listArray (0, length xs - 1) xs
              }

done cpu = _pc cpu > snd (bounds (_code cpu))

steps = unfoldr (\cpu -> if done cpu then Nothing else Just (cpu, step cpu))

run = until done step

testInput = [
    "cpy 41 a",
    "inc a",
    "inc a",
    "dec a",
    "jnz a 2",
    "dec a"
  ]

test = steps (cpu0 testInput)
test' = run (cpu0 testInput)

main1 = readFile "input.txt" >>= print . run . cpu0 . lines

cpu2 xs = let cpu = cpu0 xs in cpu { _reg = _reg cpu // [(2, 1)] }

main2 = readFile "input.txt" >>= print . run . cpu2 . lines

main = main1 >> main2
