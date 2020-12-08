{-# LANGUAGE ViewPatterns, RecordWildCards #-} 
import Data.Array
import Data.Char
import Data.Maybe
import Data.Monoid
import qualified Data.IntSet as IntSet
import Data.IntSet (IntSet)


data Opcode
    = Acc !Int
    | Nop !Int
    | Jmp !Int
  deriving (Show, Eq)

data VM = VM
    { pc :: !Int
    , program :: Array Int Opcode
    , trace :: IntSet
    , acc :: !Int
    }
  deriving (Show)

decode :: String -> Opcode
decode (words -> ["acc", readSigned -> a]) = Acc a
decode (words -> ["nop", readSigned -> a]) = Nop a
decode (words -> ["jmp", readSigned -> a]) = Jmp a
decode s = error $ "no decode: " ++ s

readSigned :: String -> Int
readSigned ('+':s) = read s
readSigned ('-':s) = -read s

data Step
    = Step VM
    | StepLoop
    | StepHalt

step :: VM -> Step
step m@VM{..}
    | IntSet.member pc trace = StepLoop
    | not (inRange (bounds program) pc) = StepHalt
    | otherwise = case program ! pc of
        Acc a -> Step $ m {pc = pc + 1, trace = trace', acc = acc + a}
        Nop a -> Step $ m {pc = pc + 1, trace = trace'}
        Jmp a -> Step $ m {pc = pc + a, trace = trace'}
  where trace' = IntSet.insert pc trace

-- Effect of running the machine
data Effect
    = Loop
    | Halt
  deriving (Eq, Ord, Show)

run :: VM -> (Int, Effect)
run m = case step m of
            Step m -> run m
            StepLoop -> (acc m, Loop)
            StepHalt -> (acc m, Halt)

-- Run until the machine halts
halts :: VM -> Maybe Int
halts m = case run m of
              (acc, Loop) -> Nothing
              (acc, Halt) -> Just acc

repair :: VM -> Maybe Int
repair m@VM{..} =
    getFirst $ foldMap (First . halts) permutations
  where
    permutations = [ m {program = program // [(i, swap (program ! i))]} 
                   | i <- range (bounds program)
                   ]

-- Swap nop and jmp
swap (Nop a) = Jmp a
swap (Jmp a) = Nop a
swap x       = x

main = do
    input <- readFile "input.txt"
    let program = map decode (lines input)
        m = VM { pc = 0
               , program = listArray (0, length program - 1) program
               , trace = mempty
               , acc = 0
               }

    -- Part 1
    let (acc, Loop) = run m
    print acc

    -- Part 2
    let Just acc = repair m
    print acc
