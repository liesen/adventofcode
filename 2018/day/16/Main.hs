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


newtype Registers = R (Array Int Int) deriving (Eq, Show)

newtype Instruction = I [Int] deriving (Show)

data Opname =
      Addr
    | Addi
    | Mulr
    | Muli
    | Banr
    | Bani
    | Borr
    | Bori
    | Setr
    | Seti
    | Gtir
    | Gtri
    | Gtrr
    | Eqir
    | Eqri
    | Eqrr
    deriving (Eq, Show)

type InstructionSet = Map Int Opname

data Sample = Sample
    { before :: Registers
    , instruction :: Instruction
    , after :: Registers
    } deriving (Show)

data Input = Input
    { samples :: [Sample]
    , program :: [Instruction]
    }

parseSample = do
    before <- fmap (R . listArray (0, 3)) $ string "Before: " *> between (char '[') (char ']') (sepBy parseNum (string ", ")) <* char '\n'
    instruction <- parseInstruction <* char '\n'
    after <- fmap (R . listArray (0, 3)) $ string "After:  " *> between (char '[') (char ']') (sepBy parseNum (string ", "))
    return $ Sample before instruction after
  where

parseNum = read <$> munch1 isDigit

parseInstruction = I <$> sepBy parseNum (char ' ') 

parse = do
    samples <- sepBy parseSample (string "\n\n")
    _ <- string "\n\n\n\n"
    program <- many (parseInstruction <* char '\n')
    eof
    return $ Input samples program

eval :: Opname -> Registers -> Instruction -> Registers
eval Addr (R reg) (I [opcode, a, b, c]) = R (reg // [(c, reg ! a + reg ! b)])
eval Addi (R reg) (I [opcode, a, b, c]) = R (reg // [(c, reg ! a + b)])
eval Mulr (R reg) (I [opcode, a, b, c]) = R (reg // [(c, reg ! a * reg ! b)])
eval Muli (R reg) (I [opcode, a, b, c]) = R (reg // [(c, reg ! a * b)])
eval Banr (R reg) (I [opcode, a, b, c]) = R (reg // [(c, reg ! a .&. reg ! b)])
eval Bani (R reg) (I [opcode, a, b, c]) = R (reg // [(c, reg ! a .&. b)])
eval Borr (R reg) (I [opcode, a, b, c]) = R (reg // [(c, reg ! a .|. reg ! b)])
eval Bori (R reg) (I [opcode, a, b, c]) = R (reg // [(c, reg ! a .|. b)])
eval Setr (R reg) (I [opcode, a, b, c]) = R (reg // [(c, reg ! a)])
eval Seti (R reg) (I [opcode, a, b, c]) = R (reg // [(c, a)])
eval Gtir (R reg) (I [opcode, a, b, c]) = R (reg // [(c, fromEnum $ a > reg ! b)])
eval Gtri (R reg) (I [opcode, a, b, c]) = R (reg // [(c, fromEnum $ reg ! a > b)])
eval Gtrr (R reg) (I [opcode, a, b, c]) = R (reg // [(c, fromEnum $ reg ! a > reg ! b)])
eval Eqir (R reg) (I [opcode, a, b, c]) = R (reg // [(c, fromEnum $ a == reg ! b)])
eval Eqri (R reg) (I [opcode, a, b, c]) = R (reg // [(c, fromEnum $ reg ! a == b)])
eval Eqrr (R reg) (I [opcode, a, b, c]) = R (reg // [(c, fromEnum $ reg ! a == reg ! b)])

testSample :: Sample -> [Opname]
testSample (Sample reg ins reg') =
    filter (\opname -> eval opname reg ins == reg') $
        [ Addr
        , Addi
        , Mulr
        , Muli
        , Banr
        , Bani
        , Borr
        , Bori
        , Setr
        , Seti
        , Gtir
        , Gtri
        , Gtrr
        , Eqir
        , Eqri
        , Eqrr
        ]

determineInstructionSet :: [Sample] -> InstructionSet
determineInstructionSet = go Map.empty
  where
    go opcodes []      = opcodes
    go opcodes samples = let (samples', opcodes') = partitionEithers $ map (f opcodes) samples
                         in go (Map.union opcodes (Map.fromList opcodes')) samples'
    f opcodes s@(Sample _ (I [opcode, _, _, _]) _) =
        case testSample s \\ Map.elems opcodes of
            [opname] -> Right (opcode, opname)
            _        -> Left s

run :: InstructionSet -> Registers -> [Instruction] -> Registers
run instructionSet reg = foldl step reg
  where
    step reg ins@(I [opcode, a, b, c]) =
        case Map.lookup opcode instructionSet of
            Nothing -> error $ "invalid opcode: " ++ show opcode
            Just opname -> eval opname reg ins

main = do
    input <- readFile "input.txt"
    let [(Input samples program, _)] = readP_to_S parse input

    -- Part 1
    print $ length $ filter ((>= 3) . length) $ map testSample samples

    -- Part 2
    let instructionSet = determineInstructionSet samples
        reg0 = R $ listArray (0, 3) [0, 0, 0, 0]
        R reg = run instructionSet reg0 program
    print $ reg ! 0
