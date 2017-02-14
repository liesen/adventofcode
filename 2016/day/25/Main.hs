{-# LANGUAGE ViewPatterns, LambdaCase #-}
import Data.Array
import Data.Char
import Data.List

import Debug.Trace

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

{-
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

-- readFile "input.txt" >>= \input -> let code = lines input in mapM_ (putStrLn . show') $ map (\n -> (n, run (cpuA n code))) [6..10]
-}

stepFile path = do
    code <- lines `fmap` readFile path
    loop (0, cpuA 10 code)
  where
    loop (maxpc, cpu)
        | done cpu = return ()
        | _pc cpu < maxpc = let (output, cpu') = step cpu
                            in case output of
                                 Just x  -> print x >> loop (max maxpc (_pc cpu'), cpu')
                                 Nothing -> loop (max maxpc (_pc cpu'), cpu')
        | otherwise = print cpu >> getChar >> loop (maxpc + 1, cpu)

step2 = stepFile "input.txt"

main2 = do
    code <- lines `fmap` readFile "input.txt"
    loop (cpuA 10 code)
  where
    loop cpu
      | done cpu = return ()
      | otherwise = let (output, cpu') = step cpu
                    in case output of
                         Just x -> print x >> loop cpu'
                         Nothing -> loop cpu'

{-
-- Then it becomes "clear" that the code produces: a! + 90^2
main2 = print (product [1..12] + 90 * 90)

main = main1 >> main2
-}

