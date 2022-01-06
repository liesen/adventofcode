{-# LANGUAGE LambdaCase, ImportQualifiedPost, ViewPatterns #-}
import Control.Monad
import Data.Char
import Data.List
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe
import Z3.Monad (Z3, MonadZ3, AST)
import Z3.Monad qualified as Z3

data Instruction
    = Inp Char
    | Add Char Val
    | Mul Char Val
    | Div Char Val
    | Mod Char Val
    | Eql Char Val
    deriving (Eq, Ord, Show)

data Val
    = Lit Int
    | Var Char
    deriving (Eq, Ord, Show)

parseInstruction :: String -> Instruction
parseInstruction (stripPrefix "inp " -> Just [a])       = Inp a
parseInstruction (stripPrefix "add " -> Just (a:' ':b)) = Add a (operand b)
parseInstruction (stripPrefix "mul " -> Just (a:' ':b)) = Mul a (operand b)
parseInstruction (stripPrefix "div " -> Just (a:' ':b)) = Div a (operand b)
parseInstruction (stripPrefix "mod " -> Just (a:' ':b)) = Mod a (operand b)
parseInstruction (stripPrefix "eql " -> Just (a:' ':b)) = Eql a (operand b)

operand ('-':x) = Lit (negate (read x))
operand [x]     | isAlpha x     = Var x
operand x       | all isDigit x = Lit (read x)

parseProgram = map parseInstruction . lines

data Env = Env
    { inps :: [AST]
    , env :: Map Char AST
    }

-- Initial environment
env0 :: Z3 Env
env0 = do
    w <- Z3.mkInteger 0
    x <- Z3.mkInteger 0
    y <- Z3.mkInteger 0
    z <- Z3.mkInteger 0
    return (Env [] (Map.fromList (zip "wxyz" [w, x, y, z])))

-- Lookup a value
val :: Map Char AST -> Val -> Z3 AST
val env (Lit i) = Z3.mkInteger (fromIntegral i)
val env (Var r) =
    case Map.lookup r env of
        Nothing -> fail "the impossible happened"
        Just r' -> return r'

-- Compile whole program, instruction by instruction. This leads to a large
-- expression which takes a long time for Z3 to evaluate. Try blockCompile
-- instead.
compile :: Env -> [Instruction] -> Z3 Env
compile env program = do
    env'@(Env inps vars) <- foldM compile1 env program
    z <- val vars (Var 'z')
    _0 <- Z3.mkInteger 0
    Z3.assert =<< Z3.mkEq z _0
    return env'

-- Compile single instruction
compile1 :: Env -> Instruction -> Z3 Env
compile1 env (Inp a)   = inp env a
compile1 env (Add a b) = binop env (\a b -> Z3.mkAdd [a, b]) a b
compile1 env (Mul a b) = binop env (\a b -> Z3.mkMul [a, b]) a b
compile1 env (Div a b) = binop env Z3.mkDiv a b
compile1 env (Mod a b) = binop env Z3.mkMod a b
compile1 env (Eql a b) = binop env eql a b

inp :: Env -> Char -> Z3 Env
inp (Env inps vars) a = do
    w <- Z3.mkFreshIntVar (a:' ':show (length inps))
    -- 1 <= w <= 9
    _1 <- Z3.mkInteger 1
    _9 <- Z3.mkInteger 9
    Z3.assert =<< Z3.mkAnd =<< sequence [Z3.mkGe w _1, Z3.mkLe w _9]
    return (Env (inps ++ [w]) (Map.insert a w vars))

binop :: Env -> (AST -> AST -> Z3 AST) -> Char -> Val -> Z3 Env
binop (Env inp env) op a' b' = do
    let Just a = Map.lookup a' env
    b <- val env b'
    a'' <- op a b
    return (Env inp (Map.insert a' a'' env))

eql :: AST -> AST -> Z3 AST
a `eql` b = join (Z3.mkIte <$> Z3.mkEq a b <*> Z3.mkInteger 0 <*> Z3.mkInteger 1)

-- Compile the expression "block by block".
{-
The input consists of 14 consecutive "blocks" that looks like this:

w = inp
x = (z % 26) + x'
z = z / z'
z = z if x == w else (z * 26) + w + y'

x' and y' are literals found in the code. So x and y are temporary
within each block.

By operating on each block the Z3 expression becomes manageable.
-}
blockCompile program = do
    z0 <- Z3.mkInteger 0
    (z, inps) <- foldM compileBlock (z0, []) (blocks program)
    -- Constrain z equal 0
    _0 <- Z3.mkInteger 0
    Z3.assert =<< Z3.mkEq z _0
    return (Env inps (Map.singleton 'z' z))
  where
    blocks [] = []
    blocks
      ( Inp 'w'
      : Mul 'x' (Lit 0)
      : Add 'x' (Var 'z')
      : Mod 'x' (Lit 26)
      : Div 'z' (Lit z)
      : Add 'x' (Lit x)
      : Eql 'x' (Var 'w')
      : Eql 'x' (Lit 0)
      : Mul 'y' (Lit 0)
      : Add 'y' (Lit 25)
      : Mul 'y' (Var 'x')
      : Add 'y' (Lit 1)
      : Mul 'z' (Var 'y')
      : Mul 'y' (Lit 0)
      : Add 'y' (Var 'w')
      : Add 'y' (Lit y)
      : Mul 'y' (Var 'x')
      : Add 'z' (Var 'y')
      : rest
      ) = (x, y, z) : blocks rest

    compileBlock :: (AST, [AST]) -> (Int, Int, Int) -> Z3 (AST, [AST])
    compileBlock (z, inps) (x', y', z') = do
        w <- Z3.mkFreshIntVar ('w':'_':show (length inps))
        -- 1 <= w <= 9
        _1 <- Z3.mkInteger 1
        _9 <- Z3.mkInteger 9
        Z3.assert =<< Z3.mkAnd =<< sequence [Z3.mkGe w _1, Z3.mkLe w _9]

        -- 
        _26 <- Z3.mkInteger 26
        x <- Z3.mkAdd =<< sequence [Z3.mkMod z _26, Z3.mkInteger (fromIntegral x')]
        z' <- Z3.mkDiv z =<< Z3.mkInteger (fromIntegral z')
        z'' <- join $ Z3.mkIte <$> Z3.mkEq x w
                               <*> pure z'
                               <*> (Z3.mkAdd =<< sequence [Z3.mkMul [z', _26], pure w, Z3.mkInteger (fromIntegral y')])
        return (z'', inps ++ [w])

optimize next constrain program = do
    Env inps _ <- blockCompile program

    -- Find one solution and optimize it digit by digit
    (res, Just model) <- Z3.solverCheckAndGetModel
    ans <- catMaybes <$> mapM (Z3.evalInt model) inps

    forM_ (zip inps ans) $ \(w, b) -> do
        let try b = do
            Z3.push
            Z3.assert =<< constrain w =<< Z3.mkInteger b
            res <- Z3.solverCheck
            Z3.pop 1
            
            case res of
                -- If no solution was found then keep the current number
                Z3.Unsat -> Z3.assert =<< Z3.mkEq w =<< Z3.mkInteger b
                -- If a solution was found then try a larger number
                Z3.Sat   -> try (next b)
        try b

    (res, Just model) <- Z3.solverCheckAndGetModel
    catMaybes <$> mapM (Z3.evalInt model) inps

maximize = optimize succ Z3.mkGt
minimize = optimize pred Z3.mkLt


main = do
    input <- readFile "input.txt"
    let program = parseProgram input

    -- Part 1
    ans1 <- Z3.evalZ3 $ maximize program
    putStrLn (map (intToDigit . fromIntegral) ans1)

    -- Part 2
    ans2 <- Z3.evalZ3 $ minimize program
    putStrLn (map (intToDigit . fromIntegral) ans2)
