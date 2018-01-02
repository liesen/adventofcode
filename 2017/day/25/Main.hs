module Main where

import Data.Array
import Data.Char
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import Text.ParserCombinators.ReadP

data State = State { _state :: Char, _slots :: Array Int Slot } deriving (Show)

data Slot = Slot { _value :: Int, _actions :: Action } deriving (Show)

data Action = Action { _write :: Int, _move :: Int, _next :: Char } deriving (Show)

data Blueprint =
      Blueprint
        { _beginState :: Char
        , _steps :: Int
        , _states :: Map Char State
        }
    deriving (Show)

blueprint :: ReadP Blueprint
blueprint = do
    skipSpaces
    _ <- string "Begin in state "
    s <- satisfy isUpper
    _ <- char '.'

    skipSpaces
    _ <- string "Perform a diagnostic checksum after "
    n <- read <$> munch1 isDigit
    _ <- string " steps."
    
    ss <- Map.fromList <$> many1 state
    skipSpaces
    eof
    return $ Blueprint s n ss

state :: ReadP (Char, State)
state = do 
    skipSpaces
    _ <- string "In state "
    s <- satisfy isUpper
    _ <- char ':'
    ss <- array (0, 1) <$> many slot
    return $ (s, State s ss)

slot :: ReadP (Int, Slot)
slot = zero +++ one
  where
    zero = p '0'
    one = p '1'
    p x = do
      skipSpaces
      _ <- string "If the current value is "
      v <- digitToInt <$> satisfy (`elem` "01")
      _ <- char ':'
      as <- actions
      return $ (v, Slot v as)

actions :: ReadP Action
actions = do
    w <- write
    m <- move
    n <- next
    return $ Action w m n

write = do
  skipSpaces
  _ <- string "- Write the value "
  v <- read <$> munch1 isDigit
  _ <- char '.'
  return $ v

move = do
  skipSpaces
  _ <- string "- Move one slot to the "
  dir <- (string "left" >> return (-1)) +++ (string "right" >> return 1)
  _ <- char '.'
  return $ dir

next = do
  skipSpaces
  _ <- string "- Continue with state "
  s <- satisfy isUpper
  _ <- char '.'
  return $ s

data Machine = Machine { _i :: Int, _tape :: Map Int Int }

instance Show Machine where
    show (Machine i tape) = "..." ++ concatMap showElem [i - 3..i + 3] ++ "..."
      where
        showElem j
            | j == i    = "[" ++ show (Map.findWithDefault 0 j tape) ++ "]"
            | otherwise = " " ++ show (Map.findWithDefault 0 j tape) ++ " "

run :: Blueprint -> Machine
run (Blueprint _beginState _steps _states) = unfoldr f (_beginState, Machine 0 mempty) !! _steps
  where
    f :: (Char, Machine) -> Maybe (Machine, (Char, Machine))
    f (s, x@(Machine i tape)) = do
          State _ _slots <- Map.lookup s _states
          let Slot _ (Action w m n) = _slots ! (Map.findWithDefault 0 i tape)
          return (x, (n, Machine (i + m) (Map.insert i w tape)))
        
checksum (Machine i tape) = sum . Map.elems $ tape

main = do
    input <- readFile "input.txt"
    let (bp, "") = head (readP_to_S blueprint input)

    -- Part 1
    let res = run bp
    print $ checksum res
