{-# LANGUAGE RecordWildCards, LambdaCase #-}
import Control.Monad
import Data.Array
import Data.Bits
import Data.Char
import Text.ParserCombinators.ReadP
import Text.Printf

{-
Key observation: the only line when reg0 is used for comparison
is line 30. It's compared against reg3. So reg0 must equal reg3
for the program to halt.
-}

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
      "#include <iostream>"
    , "#include <set>"
    , ""
    , "int main(int argc, char **argv) {"
    , "  int r[6] = {0};  // Registers"
    , "  int *ip = &r[" ++ show boundIp ++ "];  // Bind instruction pointer"
    , "  std::set<int> seen_r3;"
    , "  int prev_r3 = -1;"
    , ""
    , "  do {"
    , "    if (*ip == 28) {"
    , "      /* Part 1 */ if (seen_r3.size() == 0) std::cout << r[3] << std::endl;"
    , "      /* Part 2 */ if (seen_r3.insert(r[3]).second) { prev_r3 = r[3]; } else { std::cout << prev_r3 << std::endl; break; }"
    , "    }"
    , "    switch (*ip) {"
    ]
    ++ map (("      " ++) . genCase) (assocs code)
    ++
    [
      "    }"
    , ""
    , "    *ip += 1;"
    , "  } while (*ip >= 0 && *ip < " ++ show n ++ ");"
    , "}"
    ]
  where
    n = length code
    
    f = \case
        I "addr" a b c -> printf "r[%d] = r[%d] + r[%d];" c a b
        I "addi" a b c -> printf "r[%d] = r[%d] + %d;" c a b
        I "mulr" a b c -> printf "r[%d] = r[%d] * r[%d];" c a b
        I "muli" a b c -> printf "r[%d] = r[%d] * %d;" c a b
        I "banr" a b c -> printf "r[%d] = r[%d] & r[%d];" c a b
        I "bani" a b c -> printf "r[%d] = r[%d] & %d;" c a b
        I "borr" a b c -> printf "r[%d] = r[%d] | r[%d];" c a b
        I "bori" a b c -> printf "r[%d] = r[%d] | %d;" c a b
        I "setr" a b c -> printf "r[%d] = r[%d];" c a
        I "seti" a b c -> printf "r[%d] = %d;" c a
        I "gtir" a b c -> printf "r[%d] = %d > r[%d] ? 1 : 0;" c a b
        I "gtri" a b c -> printf "r[%d] = r[%d] > %d ? 1 : 0;" c a b
        I "gtrr" a b c -> printf "r[%d] = r[%d] > r[%d] ? 1 : 0;" c a b
        I "eqir" a b c -> printf "r[%d] = %d == r[%d] ? 1 : 0;" c a b
        I "eqri" a b c -> printf "r[%d] = r[%d] == %d ? 1 : 0;" c a b
        I "eqrr" a b c -> printf "r[%d] = r[%d] == r[%d] ? 1 : 0;" c a b

    genCase (i, x@(I op a b c)) =
      "case " ++ show i ++ ": " ++ f x ++ " break;  // " ++ show x

main = do
    input <- readFile "input.txt"
    let [(dev0, "")] = readP_to_S parse input

    -- Print C++ version of the program
    putStrLn (codegen dev0)