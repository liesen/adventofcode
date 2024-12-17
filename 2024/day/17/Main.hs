{-# LANGUAGE RecordWildCards #-}

import Data.Bits
import Data.Char
import Data.Ix
import Data.List
import Data.Vector (Vector, (!))
import Data.Vector qualified as Vector
import Text.ParserCombinators.ReadP

parse :: ReadP (Vector Int, Cpu)
parse = do
  a <- read <$> (string "Register A: " *> munch isDigit <* nl)
  b <- read <$> (string "Register B: " *> munch isDigit <* nl)
  c <- read <$> (string "Register C: " *> munch isDigit <* nl)
  _ <- nl
  prog <- (Vector.fromList . map digitToInt) <$> (string "Program: " *> sepBy (satisfy isDigit) (char ',') <* nl)
  return $ (prog, Cpu 0 a b c [])
  where
    nl = char '\n'

data Opcode
  = Adv
  | Bxl
  | Bst
  | Jnz
  | Bxc
  | Out
  | Bdv
  | Cdv

data Cpu = Cpu {ip :: Int, a :: Int, b :: Int, c :: Int, out :: [Int]} deriving (Eq, Ord, Show)

data Step a = Step a | Done a

step :: Vector Int -> Cpu -> Maybe Cpu
step prog cpu@Cpu {..}
  | ip >= Vector.length prog = Nothing
  | otherwise =
      Just $ case (prog ! ip, prog ! (ip + 1)) of
        -- adv
        (0, operand) -> cpu {ip = ip + 2, a = a `div` (1 `shiftL` combo operand)}
        -- bxl
        (1, operand) -> cpu {ip = ip + 2, b = b `xor` operand}
        -- bst
        (2, operand) -> cpu {ip = ip + 2, b = combo operand `mod` 8}
        -- jnz
        (3, operand) | a == 0 -> cpu {ip = ip + 2}
        (3, operand) -> cpu {ip = operand}
        -- bxc
        (4, operand) -> cpu {ip = ip + 2, b = b `xor` c}
        -- out
        (5, operand) -> cpu {ip = ip + 2, out = combo operand `mod` 8 : out}
        -- bdv
        (6, operand) -> cpu {ip = ip + 2, b = a `div` (1 `shiftL` combo operand)}
        -- cdv
        (7, operand) -> cpu {ip = ip + 2, c = a `div` (1 `shiftL` combo operand)}
  where
    combo 0 = 0
    combo 1 = 1
    combo 2 = 2
    combo 3 = 3
    combo 4 = a
    combo 5 = b
    combo 6 = c
    combo 7 = error "invalid program"

-- run prog = unfoldr (step prog)
steps prog cpu =
  cpu : case step prog cpu of
    Nothing -> []
    Just cpu' -> steps prog cpu'

run prog cpu = last $ steps prog cpu

prog = Vector.fromList [0, 1, 2, 3]

test = step prog (Cpu 0 0 0 0 [])

tests =
  [ step (Vector.fromList [2, 6]) (Cpu 0 0 0 9 []),
    Just $ run (Vector.fromList [5, 0, 5, 1, 5, 4]) (Cpu 0 10 0 0 []),
    Just $ run (Vector.fromList [0, 1, 5, 4, 3, 0]) (Cpu 0 2024 0 0 [])
  ]

example = run prog (Cpu 0 729 0 0 [])
  where
    prog = Vector.fromList [0, 1, 5, 4, 3, 0]

main = do
  input <- readFile "input"
  let [((prog, cpu), "")] = readP_to_S (parse <* skipSpaces <* eof) input

  -- Part 1
  let cpu1 = run prog cpu
      out1 = out cpu1
      ans1 = intersperse ',' (map intToDigit (reverse out1))
  putStrLn ans1