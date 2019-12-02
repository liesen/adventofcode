{-# LANGUAGE RecordWildCards #-}
import Control.Monad
import Control.Monad.Except
import Data.Array
import Data.List
import Data.List.Split
import Debug.Trace

-- Idea is to compute the final expression w.r.t. to the variables, verb and
-- noun, and then to evaluate it to find the combination that produces the
-- value we look for.

-- Expression tree
data Expr
    = Lit Int
    | Verb
    | Noun
    | Add Expr Expr
    | Mul Expr Expr

-- Evaluation
data Env = Env { verb :: Int, noun :: Int }

eval env     (Lit n)  = n
eval Env{..} Verb = verb
eval Env{..} Noun = noun
eval env     (Add a b) = eval env a + eval env b
eval env     (Mul a b) = eval env a * eval env b

showExpr :: Expr -> String
showExpr (Lit n) = show n
showExpr Verb = "verb"
showExpr Noun = "noun"
showExpr (Add a b) = showExpr a ++ " + " ++ showExpr b
showExpr (Mul a b) = showFactor a ++ " * " ++ showFactor b

showFactor :: Expr -> String
showFactor (Add a b) = "(" ++ showExpr (Add a b) ++ ")"
showFactor e = showExpr e

instance Show Expr where
    show = showExpr

instance Num Expr where
    fromInteger = Lit . fromInteger
    (+) = Add
    (*) = Mul
    negate = error "not implemented"
    abs = error "not implemented"
    signum = error "not implemented"

-- Intcode memory now stores the value and the expression that was used to
-- calculate it
data Intcode = Intcode Int (Array Int (Int, Expr))
    deriving Show

data Intstep = Halt
             | Cont Intcode

step :: Intcode -> Except String Intstep
step (Intcode pc mem) =
    case opcode of
        99 -> return Halt
        1 -> 
            let (m, a) = mem ! fst (mem ! (pc + 1))
                (n, b) = mem ! fst (mem ! (pc + 2))
                output = fst (mem ! (pc + 3))
            in return $ Cont (Intcode (pc + 4) (mem // [(output, (m + n, a + b))]))
        2 ->
            let (m, a) = mem ! fst (mem ! (pc + 1))
                (n, b) = mem ! fst (mem ! (pc + 2))
                output = fst (mem ! (pc + 3))
            in return $ Cont (Intcode (pc + 4) (mem // [(output, (m * n, a * b))]))
        _ -> throwError "unknown opcode"
  where
    opcode = fst (mem ! pc)

run :: Bool -> Intcode -> Except String Intcode
run trace p = do
    s <- if trace then traceShow p (step p) else step p
    
    case s of
      Halt -> return p
      Cont p' -> run trace p'

main = do
    input <- readFile "input.txt"
    let program = map (\i -> (i, Lit i)) $ map read $ splitOn "," input
        mem0 = listArray (0, length program - 1) program
    
    -- Part 1 & 2
    let mem1 = mem0 // [(1, (12, Verb)), (2, (2, Noun))]
        intcode = Intcode 0 mem1
    case runExcept (run False intcode) of
        Left err -> print $ "Error: " ++ err
        Right (Intcode pc res) -> 
            let (ans1, expr) = res ! 0
                [ans2] = [ 100 * verb + noun
                         | verb <- [0..99]
                         , noun <- [0..99]
                         , eval Env{..} expr == 19690720]
            in do
                putStrLn $ "Expr: " ++ show expr
                putStrLn $ "Part 1: " ++ show ans1  -- 2692315
                putStrLn $ "Part 2: " ++ show ans2  -- 9507

