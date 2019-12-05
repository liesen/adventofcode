{-# LANGUAGE RecordWildCards #-}
import Control.Monad
import Control.Monad.Except
import Data.Array
import Data.List
import Data.List.Split
import Debug.Trace

data Intcode
    = Intcode { pc :: Int
              , mem :: Array Int Int
              , input :: [Int]
              , output :: [Int]
              }
  deriving Show

data Intstep = Halt
             | Cont Intcode

step :: Intcode -> Except String Intstep
step program@Intcode{..} =
    case opcode of
        99 -> return Halt
        1 -> 
            return $ Cont (program { pc = pc + 4, mem = mem // [(mem ! (pc + 3), param 1 + param 2)] })
        2 ->
            return $ Cont (program { pc = pc + 4, mem = mem // [(mem ! (pc + 3), param 1 * param 2)] })
        3 ->
            let x:input' = input
            in return $ Cont $ program { pc = pc + 2, mem = mem // [(mem ! (pc + 1), x)] }
        4 ->
            return $ Cont $ program { pc = pc + 2, output = output ++ [param 1]}
        5 -> -- jump-if-true
            case param 1 of
                0 -> return $ Cont $ program { pc = pc + 3 }
                _ -> return $ Cont $ program { pc = param 2 }
        6 -> -- jump-if-false
            case param 1 of
                0 -> return $ Cont $ program { pc = param 2 }
                _ -> return $ Cont $ program { pc = pc + 3 }
        7 -> -- less than
            return $ Cont $ program { pc = pc + 4, mem = mem // [(mem ! (pc + 3), if param 1 < param 2 then 1 else 0)] }
        8 -> -- equals
            return $ Cont $ program { pc = pc + 4, mem = mem // [(mem ! (pc + 3), if param 1 == param 2 then 1 else 0)] }

        _ -> throwError ("unknown opcode" ++ show opcode ++ " at pc=" ++ show pc)
  where
    opcode = (mem ! pc) `mod` 100

    param i = get mode (pc + i)
      where mode = ((mem ! pc) `div` (10 * 10^i)) `mod` 10

    get 0 addr = mem ! (mem ! addr)
    get 1 addr = mem ! addr

    pos = get 0
    imm = get 1

run :: Bool -> Intcode -> Except String Intcode
run trace p = do
    s <- if trace then traceShow p (step p) else step p
    
    case s of
      Halt -> return p
      Cont p' -> run trace p'

main = do
    input <- readFile "input.txt"

    let program = map read $ splitOn "," input
        mem0 = listArray (0, length program - 1) program

    -- Part 1
    case runExcept (run False (Intcode { pc = 0, mem = mem0, input = [1], output = []})) of
      Right (Intcode{..}) ->
        putStrLn $ show (last output)

    -- Part 2
    case runExcept (run False (Intcode { pc = 0, mem = mem0, input = [5], output = []})) of
      Right (Intcode{..}) ->
        putStrLn $ show (last output)
