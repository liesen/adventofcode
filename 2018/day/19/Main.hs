{-# LANGUAGE ViewPatterns, RecordWildCards, LambdaCase #-}
import Control.Monad
import Data.Array
import Data.Bits
import Data.Char
import Data.Either
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Text.ParserCombinators.ReadP
import Text.Printf

import Debug.Trace

data Instruction = I String Int Int Int deriving (Eq)

instance Show Instruction where
    show (I opname a b c) = printf "%s %d %d %d" opname a b c

data Device = Dev
    { boundIp :: Int
    , ip :: Int
    , reg :: Array Int Int
    , code :: Array Int Instruction
    } deriving (Show)

parse = do
    boundIp <- string "#ip " *> parseNum <* char '\n'
    xs <- sepBy1 parseInstruction (char '\n')
    optional (char '\n')
    eof
    return $ Dev boundIp 0 (listArray (0, 5) (repeat 0)) (listArray (0, length xs - 1) xs)

parseNum = read <$> munch1 isDigit
   
parseInstruction = do
    opname <- count 4 get
    [a, b, c] <- count 3 (char ' ' *> parseNum)
    return $ I opname a b c
              
halted Dev{..} = not (inRange (bounds code) ip)

eval Dev{..} =
    case opname of
        "addr" -> step $ \reg a b c -> reg // [(c, reg ! a + reg ! b)]
        "addi" -> step $ \reg a b c -> reg // [(c, reg ! a + b)]
        "mulr" -> step $ \reg a b c -> reg // [(c, reg ! a * reg ! b)]
        "muli" -> step $ \reg a b c -> reg // [(c, reg ! a * b)]
        "banr" -> step $ \reg a b c -> reg // [(c, reg ! a .&. reg ! b)]
        "bani" -> step $ \reg a b c -> reg // [(c, reg ! a .&. b)]
        "borr" -> step $ \reg a b c -> reg // [(c, reg ! a .|. reg ! b)]
        "bori" -> step $ \reg a b c -> reg // [(c, reg ! a .|. b)]
        "setr" -> step $ \reg a b c -> reg // [(c, reg ! a)]
        "seti" -> step $ \reg a b c -> reg // [(c, a)]
        "gtir" -> step $ \reg a b c -> reg // [(c, fromEnum $ a > reg ! b)]
        "gtri" -> step $ \reg a b c -> reg // [(c, fromEnum $ reg ! a > b)]
        "gtrr" -> step $ \reg a b c -> reg // [(c, fromEnum $ reg ! a > reg ! b)]
        "eqir" -> step $ \reg a b c -> reg // [(c, fromEnum $ a == reg ! b)]
        "eqri" -> step $ \reg a b c -> reg // [(c, fromEnum $ reg ! a == b)]
        "eqrr" -> step $ \reg a b c -> reg // [(c, fromEnum $ reg ! a == reg ! b)]
  where
    I opname a b c = code ! ip
    step f = let reg' = f (reg // [(boundIp, ip)]) a b c
             in Dev boundIp ((reg' ! boundIp) + 1) reg' code

codegen :: Device -> String
codegen Dev{..} = unlines $
    [
      "#include <stdio.h>"
    , ""
    , "int main(int argc, char **argv) {"
    , "  int r0 = 0, r1 = 0, r2 = 0, r3 = 0, r4 = 0, r5 = 0;  // Registers"
    , "  int *ip = &r1;  // Bind instruction pointer to register 1"
    , ""
    , "  while (*ip < " ++ show n ++ ") {"
    , "    switch (*ip) {"
    ]
    ++ map ("      " ++ ) (map genCase (assocs code))
    ++
    [
      "    }"
    , ""
    , "    *ip += 1;"
    , "  }"
    , ""
    , "  printf(\"%d\\n\", r0);"
    , "}"
    ]
  where
    n = length code
    
    f = \case
        I "addr" a b c -> printf "r%d = r%d + r%d;" c a b
        I "addi" a b c -> printf "r%d = r%d + %d;" c a b
        I "mulr" a b c -> printf "r%d = r%d * r%d;" c a b
        I "muli" a b c -> printf "r%d = r%d * %d;" c a b
        I "banr" a b c -> printf "r%d = r%d & r%d;" c a b
        I "bani" a b c -> printf "r%d = r%d & %d;" c a b
        I "borr" a b c -> printf "r%d = r%d | r%d;" c a b
        I "bori" a b c -> printf "r%d = r%d | %d;" c a b
        I "setr" a b c -> printf "r%d = r%d;" c a
        I "seti" a b c -> printf "r%d = %d;" c a
        I "gtir" a b c -> printf "r%d = %d > r%d;" c a b
        I "gtri" a b c -> printf "r%d = r%d > %d;" c a b
        I "gtrr" a b c -> printf "r%d = r%d > r%d;" c a b
        I "eqir" a b c -> printf "r%d = %d == r%d;" c a b
        I "eqri" a b c -> printf "r%d = r%d == %d;" c a b
        I "eqrr" a b c -> printf "r%d = r%d == r%d;" c a b

    genCase (i, x@(I op a b c)) =
      "case " ++ show i ++ ": " ++ f x ++ " break;"

main = do
    input <- readFile "input.txt"
    let [(dev0, "")] = readP_to_S parse input

    -- Print C version of the program
    -- putStrLn (codegen dev0)

    -- Part 1
    print $ (! 0) . reg $ until halted eval dev0

    -- Part 2
    -- let dev1 = dev0 { reg = reg dev0 // [(0, 1)] }
    -- print $ (! 0) . reg $ until halted eval dev0
  
    -- Print C version of the program
    -- putStrLn (codegen dev0)
    let r5 = 10551296
    print $ r5 + sum [d | d <- [1..r5 `div` 2], r5 `mod` d == 0]


    -- mapM_ (uncurry debug) $ sample 10551296 $ zip [0..] $ iterate eval dev1

    -- plotRun dev1

