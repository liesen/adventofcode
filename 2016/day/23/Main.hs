{-# LANGUAGE ViewPatterns, LambdaCase #-}
import Data.Array
import Data.Char
import Data.List

import Debug.Trace

--- Day 23: Safe Cracking ---
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
        cpu { _reg = reg // [(r, x)], _pc = pc + 1 }
      ["inc", (v -> (R r, x))] ->
        cpu { _reg = reg // [(r, x + 1)], _pc = pc + 1 }
      ["dec", (v -> (R r, x))] ->
        cpu { _reg = reg // [(r, x - 1)], _pc = pc + 1 }
      ["jnz", (v -> (_, 0)), _] ->
        cpu { _pc = pc + 1 }
      ["jnz", (v -> _), (v -> (_, y))] ->
        cpu { _pc = pc + y }
      ["tgl", (v -> (_, x))] | inRange (bounds code) (pc + x) ->
        cpu { _code = code // [(pc + x, tgl (code ! (pc + x)))], _pc = pc + 1 }
      -- ["mul", (v -> (R r, x)), (v -> (_, y))] ->
      --  cpu { _reg = reg // [(r, x * y)], _pc = pc + 1 }
      _ -> {- trace ("invalid instruction: " ++ show (code ! pc)) $ -} cpu { _pc = pc + 1}
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

cpu0 xs = Cpu { _reg = listArray (0, 3) [7, 0, 0, 0],
                _pc = 0,
                _code = listArray (0, length xs - 1) xs
              }

done cpu = _pc cpu > snd (bounds (_code cpu))

steps = unfoldr (\cpu -> if done cpu then Nothing else Just (cpu, step cpu))

run = until done step

testInput = [
    "cpy 2 a",
    "tgl a",
    "tgl a",
    "tgl a",
    "cpy 1 a",
    "dec a",
    "dec a"
  ]

test = steps (cpu0 testInput)
test' = run (cpu0 testInput)

runFile path = readFile path >>= return . run . cpu0 . lines

main1 = runFile "input.txt" >>= print . (! 0) . _reg

--- Part Two ---

-- Get result for the first couple of A:s
--
-- {6,8820},{7,13140},{8,48420},{9,370980},{10,3636900}
-- 
cpuA n xs = let cpu = cpu0 xs in cpu { _reg = _reg cpu // [(0, n)] }

-- readFile "input.txt" >>= \input -> let code = lines input in mapM_ (putStrLn . show') $ map (\n -> (n, run (cpuA n code))) [6..10]

stepFile path = do
    code <- lines `fmap` readFile path
    loop (0, cpu0 code)
  where
    step' = until (\(maxpc, _pc -> pc) -> pc >= maxpc) (\(maxpc, cpu) -> let cpu' = step cpu in (max maxpc (_pc cpu'), cpu'))

    loop (maxpc, cpu)
        | done cpu  = return ()
        | otherwise = let (maxpc', cpu') = step' (maxpc, cpu)
                      in print cpu' >> getChar >> loop (maxpc' + 1, cpu')

step2 = stepFile "input.txt"

-- Then it becomes "clear" that the code produces: a! + 90^2
main2 = print (product [1..12] + 90 * 90)

main = main1 >> main2
