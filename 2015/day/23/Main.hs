{-# LANGUAGE ViewPatterns, RecordWildCards #-} 
import Data.Array
import Data.List

data VM = VM
    { pc :: Int
    , prog :: Array Int Opcode
    , reg :: Array Char Int
    } deriving (Show)

data Opcode
    = Hlf !Char
    | Tpl !Char
    | Inc !Char
    | Jmp !Int
    | Jie !Char !Int
    | Jio !Char !Int
  deriving (Show, Eq)

decode (words -> ["hlf", [r]]) = Hlf r
decode (words -> ["tpl", [r]]) = Tpl r
decode (words -> ["inc", [r]]) = Inc r
decode (words -> ["jmp", offset -> o]) = Jmp o
decode (words -> ["jie", r:",", offset -> o]) = Jie r o
decode (words -> ["jio", r:",", offset -> o]) = Jio r o

offset ('+':(read -> n)) = n
offset ('-':(read -> n)) = -n

halted VM{..} = not (inRange (bounds prog) pc)

step m@VM{..} =
    case prog ! pc of
        Hlf r -> m { reg = reg // [(r, (reg ! r) `div` 2)], pc = pc + 1 }
        Tpl r -> m { reg = reg // [(r, (reg ! r) * 3)], pc = pc + 1 }
        Inc r -> m { reg = reg // [(r, (reg ! r) + 1)], pc = pc + 1 }
        Jmp offset -> m { pc = pc + offset }
        Jie r offset ->
            if even (reg ! r)
                then m { pc = pc + offset }
                else m { pc = pc + 1 }
        Jio r offset ->
            if reg ! r == 1
                then m { pc = pc + offset }
                else m { pc = pc + 1 }

main = do
    input <- readFile "input.txt"
    let instructions = lines input
        vm = VM { pc = 0
                , prog = listArray (0, length instructions - 1) (map decode instructions) 
                , reg = listArray ('a', 'b') [0, 0]
                }

    -- Part 1
    print $ (! 'b') $ reg $ until halted step vm

    -- Part 2
    print $ (! 'b') $ reg $ until halted step (vm { reg = reg vm // [('a', 1)] })
    