import Control.Monad
import Control.Monad.Except
import Data.Array
import Data.List
import Data.List.Split
import Debug.Trace

data Intcode = Intcode Int (Array Int Int)
    deriving Show

data Intstep = Halt
             | Cont Intcode

step :: Intcode -> Except String Intstep
step (Intcode pc mem) =
    case opcode of
        99 -> return Halt
        1 -> 
            let a = mem ! (mem ! (pc + 1))
                b = mem ! (mem ! (pc + 2))
                output = mem ! (pc + 3)
            in return $ Cont (Intcode (pc + 4) (mem // [(output, a + b)]))
        2 ->
            let a = mem ! (mem ! (pc + 1))
                b = mem ! (mem ! (pc + 2))
                output = mem ! (pc + 3)
            in return $ Cont (Intcode (pc + 4) (mem // [(output, a * b)]))
        _ -> throwError "unknown opcode"
  where
    opcode = mem ! pc

run :: Bool -> Intcode -> Except String Intcode
run trace p = do
    s <- if trace then traceShow p (step p) else step p
    
    case s of
      Halt -> return p
      Cont p' -> run trace p'

main = do
    input <- readFile "input.txt"
    
    -- Test
    case runExcept (run False (Intcode 0 (listArray (0, 11) [1,9,10,3,2,3,11,0,99,30,40,50]))) of
      Right (Intcode _ mem) ->
        putStrLn $ "Test: " ++ show (mem ! 0)
    
    let program = map read $ splitOn "," input
        mem0 = listArray (0, length program - 1) program

    -- Part 1
    let mem1 = mem0 // [(1, 12), (2, 2)]
        intcode = Intcode 0 mem1
    case runExcept (run False intcode) of
        Left err -> print $ "Error: " ++ err
        Right (Intcode pc res) -> 
            let ans1 = res ! 0
            in putStrLn $ "Part 1: " ++ show ans1  -- 2692315

    -- Part 2
    -- Just brute force it...
    let [ans2] = [ (100 * noun + verb)
                 | noun <- [0..99]
                 , verb <- [0..99]
                 , let mem2 = mem0 // [(1, noun), (2, verb)]
                 , let Right (Intcode _ res) = runExcept (run False (Intcode 0 mem2))
                 , res ! 0 == 19690720
                 ]
    putStrLn $ "Part 2: " ++ show ans2 -- 9507