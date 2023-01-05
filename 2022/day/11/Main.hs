{-# LANGUAGE Strict, ImportQualifiedPost, RecordWildCards #-}
import Control.Monad
import Data.Char
import Data.Functor
import Data.List
import Data.Map (Map)
import Data.Map.Strict qualified as Map
import Data.Ord
import Text.ParserCombinators.ReadP


data Operation
    = Mul Val
    | Add Val
  deriving (Show)

data Val
    = Old
    | Lit Integer
  deriving (Show)

data Monkey = Monkey
    { index :: Int,
      items :: [Integer],
      operation :: Operation,
      test :: Integer,
      ifTrue :: Int,
      ifFalse :: Int,
      inspections :: Int
    }
  deriving (Show)

parseMonkey = do
    index <- string "Monkey " *> num <* string ":\n"
    items <- string "  Starting items: " *> sepBy num (string ", ") <* char '\n'
    operation <- string "  Operation: " *> parseOperation <* char '\n'
    test <- string "  Test: divisible by " *> num <* char '\n'
    ifTrue <- string "    If true: throw to monkey " *> num <* char '\n'
    ifFalse <- string "    If false: throw to monkey " *> num
    let inspections = 0
    return Monkey{..}

num :: (Read a, Num a) => ReadP a
num = read <$> munch1 isDigit

parseOperation :: ReadP Operation
parseOperation = string "new = old " *> (mul +++ add)

mul, add :: ReadP Operation
mul = Mul <$> (string "* " *> val)
add = Add <$> (string "+ " *> val)

val :: ReadP Val
val = (string "old" $> Old) +++ (Lit <$> num)

eval :: Operation -> Integer -> Integer
eval (Mul Old) old = old * old
eval (Mul (Lit n)) old = old * n
eval (Add Old) old = old + old
eval (Add (Lit n)) old = old + n

monkeyRound :: (Integer -> Integer) -> [Int] -> Map Int Monkey -> Map Int Monkey
monkeyRound relief order monkeys = foldl (monkeyTick relief) monkeys order

monkeyTick :: (Integer -> Integer) -> Map Int Monkey -> Int -> Map Int Monkey
monkeyTick relief monkeys i =
    Map.adjust adjust i $ foldl (monkeyItemTick relief monkey) monkeys (items monkey)
  where
    Just monkey = Map.lookup i monkeys
    adjust m = m {items = [], inspections = inspections m + length (items monkey)}

monkeyItemTick :: (Integer -> Integer) -> Monkey -> Map Int Monkey -> Integer -> Map Int Monkey
monkeyItemTick relief monkey monkeys worryLevel =
    Map.adjust (\monkey' -> monkey' { items = items monkey' ++ [worryLevel'']}) target monkeys
  where
    worryLevel' = eval (operation monkey) worryLevel
    worryLevel'' = relief worryLevel'
    res = worryLevel'' `mod` test monkey == 0
    target = if res then ifTrue monkey else ifFalse monkey

monkeyBusiness = product . take 2 . reverse . sort . map inspections . Map.elems

main = do
    input <- readFile "input.txt"
    let [(monkeys, "")] = readP_to_S (endBy parseMonkey skipSpaces <* eof) input
        indices = map index monkeys
        monkeyMap = Map.fromList (zip indices monkeys)

    -- Part 1
    let relief1 worryLevel = worryLevel `div` 3
        monkeys1 = iterate (monkeyRound relief1 indices) monkeyMap !! 20

    print $ monkeyBusiness monkeys1

    -- Part 2
    let interval = product $ map test monkeys
        relief2 worryLevel = worryLevel `mod` interval
        monkeys2 = iterate (monkeyRound relief2 indices) monkeyMap !! 10000

    print $ monkeyBusiness monkeys2
