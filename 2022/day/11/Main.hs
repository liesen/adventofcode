{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ImportQualifiedPost #-}
import Text.ParserCombinators.ReadP
import Data.Char
import Data.Functor
import Data.Map qualified as Map
import Data.Map (Map)

import Debug.Trace


data Operation
    = Mul Val
    | Add Val
  deriving (Show)

data Val
    = Old
    | Lit Int
  deriving (Show)

data Monkey = Monkey
    { index :: Int,
      items :: [Int],
      operation :: Operation,
      test :: Int,
      ifTrue :: Int,
      ifFalse :: Int
    }
  deriving (Show)

parseMonkey = do
    index <- string "Monkey " *> num <* string ":\n"
    items <- string "  Starting items: " *> sepBy num (string ", ") <* char '\n'
    operation <- string "  Operation: " *> parseOperation <* char '\n'
    test <- string "  Test: divisible by " *> num <* char '\n'
    ifTrue <- string "    If true: throw to monkey " *> num <* char '\n'
    ifFalse <- string "    If false: throw to monkey " *> num
    return Monkey{..}

num :: ReadP Int
num = read <$> munch1 isDigit

parseOperation :: ReadP Operation
parseOperation = string "new = old " *> (mul +++ add)

mul, add :: ReadP Operation
mul = Mul <$> (string "* " *> val)
add = Add <$> (string "+ " *> val)

val :: ReadP Val
val = (string "old" $> Old) +++ (Lit <$> num)

eval :: Operation -> Int -> Int
eval (Mul Old) old = old * old
eval (Mul (Lit n)) old = old * n
eval (Add Old) old = old + old
eval (Add (Lit n)) old = old + n

monkeyRound :: [Int] -> Map Int Monkey -> Map Int Monkey
monkeyRound order monkeys = foldl monkeyTick monkeys order

monkeyTick :: Map Int Monkey -> Int -> Map Int Monkey
monkeyTick monkeys i =
    -- trace ("Monkey " ++ show i ++ ":") $
    Map.adjust (\monkey' -> monkey' { items = [] }) i $ foldl (monkeyItemTick monkey) monkeys items
  where
    Just monkey@Monkey{..} = Map.lookup i monkeys

monkeyItemTick :: Monkey -> Map Int Monkey -> Int -> Map Int Monkey
monkeyItemTick monkey monkeys worryLevel =
    {-
    traceShow monkeys $
    trace ("  Monkey " ++ show (index monkey) ++ " inspects an item with worry level of " ++ show worryLevel ++ ".") $
    trace ("    Worry level is set to " ++ show worryLevel' ++ ".") $
    trace ("    Monkey gets bored with item. Worry level is divided by 3 to " ++ show worryLevel'' ++ ".") $
    trace ("    Current worry level is " ++ (if not res then "not " else "") ++ "divisible by " ++ show (test monkey) ++ ".") $
    trace ("    Item with worry level " ++ show worryLevel'' ++ " is thrown to monkey " ++ show target ++ ".") $
    -}
    Map.adjust (\monkey' -> monkey' { items = items monkey' ++ [worryLevel'']}) target monkeys
  where
    worryLevel' = eval (operation monkey) worryLevel
    worryLevel'' = worryLevel' `div` 3
    res = worryLevel'' `mod` test monkey == 0
    target = if res then ifTrue monkey else ifFalse monkey

main = do
    input <- readFile "input.txt"
    let [(monkeys, "")] = readP_to_S (endBy parseMonkey skipSpaces <* eof) input
        indices = map index monkeys
        monkeyMap = Map.fromList (zip indices monkeys)
        rounds = iterate (monkeyRound indices) monkeyMap

    putStrLn "After round 1:"
    print $ rounds !! 1
    print $ rounds !! 2
    print $ rounds !! 20