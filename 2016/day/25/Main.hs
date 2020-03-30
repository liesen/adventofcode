{-# LANGUAGE ViewPatterns #-}
import Data.Array
import Data.Char
import Data.List
import qualified Data.Set as Set

--- Day 25: Clock Signal ---

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
          _code :: Array Int String
        }

cpuA a xs = Cpu { _reg = listArray (0, 3) [a, 0, 0, 0],
                  _pc = 0,
                  _code = listArray (0, length xs - 1) xs
                }
cpu0 = cpuA 0

instance Show Cpu where
    show cpu = "{PC=" ++ show (_pc cpu) ++ " " ++
               unwords (map showReg (assocs (_reg cpu))) ++
               "}"
      where
        showReg (r, v) = chr (r + ord 'a'):'=':show v

show' cpu = "{PC=" ++ show (_pc cpu) ++ " " ++
            unwords (map showReg (assocs (_reg cpu))) ++
            "\n" ++
            unlines (map showCode (assocs (_code cpu))) ++
            "}"
  where
    showReg (r, v) = chr (r + ord 'a'):'=':show v
    showCode (r, v) = "  " ++ show r ++ ": " ++ v

step cpu =
    case words (code ! pc) of
      ["cpy", (v -> (_, x)), (op -> R r)] ->
        (Nothing, cpu { _reg = reg // [(r, x)], _pc = pc + 1 })
      ["inc", (v -> (R r, x))] ->
        (Nothing, cpu { _reg = reg // [(r, x + 1)], _pc = pc + 1 })
      ["dec", (v -> (R r, x))] ->
        (Nothing, cpu { _reg = reg // [(r, x - 1)], _pc = pc + 1 })
      ["jnz", (v -> (_, 0)), _] ->
        (Nothing, cpu { _pc = pc + 1 })
      ["jnz", (v -> _), (v -> (_, y))] ->
        (Nothing, cpu { _pc = pc + y })
      ["tgl", (v -> (_, x))] | inRange (bounds code) (pc + x) ->
        (Nothing, cpu { _code = code // [(pc + x, tgl (code ! (pc + x)))], _pc = pc + 1 })
      ["out", v -> (R _, x)] ->
        (Just x, cpu { _pc = pc + 1 })
      _ -> 
        (Nothing, cpu { _pc = pc + 1})
  where
    pc = _pc cpu
    code = _code cpu
    reg = _reg cpu

    v (op -> V x) = (V x, x)
    v (op -> R r) = (R r, _reg cpu ! r)

    tgl (words -> ["inc", x]) = unwords ["dec", x]
    tgl (words -> [_, x]) = unwords ["inc", x]
    tgl (words -> ["jnz", x, y]) = unwords ["cpy", x, y]
    tgl (words -> [_, x, y]) = unwords ["jnz", x, y]

done cpu = _pc cpu > snd (bounds (_code cpu))

findCycle maxLength xs cpu = go maxLength xs cpu mempty
  where
    go n (x:xs) cpu seen
      | Set.member rep seen = True
      | done cpu = False
      | n == 0 = True  -- this is likely a repeating sequence
      | otherwise = case step cpu of
                      (Just y, cpu')
                        | x /= y -> False -- fail "cycle mismatch"
                        | otherwise -> go (n - 1) xs cpu' (Set.insert rep seen)
                      (Nothing, cpu') -> go n (x:xs) cpu' (Set.insert rep seen)
      where
        rep = (_pc cpu, _reg cpu)

main = do
    input <- readFile "input.txt"
    let program = lines input

    -- Part 1
    let Just a = find (\a -> findCycle 100 (cycle [0, 1]) (cpuA a program)) [0..]
    print a