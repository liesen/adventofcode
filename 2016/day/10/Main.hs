{-# LANGUAGE ViewPatterns #-}

--- Day 10: Balance Bots ---
import Control.Monad
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Debug.Trace

--- Day 10: Balance Bots ---

data MinMax
    = Empty
    | Min Int
    | MinMax Int Int
  deriving (Show, Eq)

instance Semigroup MinMax where
    Empty <> x = x
    x <> Empty = x
    (Min a) <> (Min b) = MinMax (min a b) (max a b)
    (Min a) <> (MinMax b c) = MinMax (min a b) (max a c)
    (MinMax a b) <> (Min c) = MinMax (min a c) (max b c)
    (MinMax a b) <> (MinMax c d) = MinMax (min a c) (max b d)

instance Monoid MinMax where
    mempty = Empty

data Bot = Bot MinMax [String]
    deriving (Show)

instance Semigroup Bot where
  Bot x xs <> Bot y ys = Bot (x <> y) (xs <> ys)

instance Monoid Bot where
    mempty = Bot mempty mempty

data State = State { _bots :: Map Int Bot, _outputs :: Map Int Int }
    deriving (Show)

state0 = State mempty mempty

step :: State -> String -> IO State
step s x@(words -> ["value", v, "goes", "to", "bot", b]) =
    let bot = read b
        value = read v
    in process bot $ s { _bots = Map.insertWith mappend bot (Bot (Min value) []) (_bots s) }
step s x@(words -> [
          "bot", src,
          "gives", "low", "to", dstLow, low,
          "and", "high", "to", dstHigh, high]) =
    case Map.lookup (read src) (_bots s) of
      Just (Bot (MinMax a b) xs) -> do
        when (a == 17 && b == 61) $ putStrLn src -- This is the bot comparing 17 to 61
        s' <- give s dstLow (read low) a
        s'' <- give s' dstHigh (read high) b
        return $ s'' { _bots = Map.insert (read src) mempty (_bots s'') }
      Just (Bot minmax xs) ->
        return $ s { _bots = Map.insert (read src) (Bot minmax (xs ++ [x])) (_bots s) }
      Nothing ->
        return $ s { _bots = Map.insert (read src) (Bot Empty [x]) (_bots s) }

give :: State -> String -> Int -> Int -> IO State
give s "bot" bot value = process bot $ s { _bots = Map.insertWith mappend bot (Bot (Min value) []) (_bots s) }
give s "output" bot value = return $ s { _outputs = Map.insert bot value (_outputs s) }

-- Do a bot's actions once it's min-max bucket is filled
process bot s@(Map.lookup bot . _bots -> Just (Bot (MinMax _ _) xs)) = foldM step s xs
process bot s = return s

run :: [String] -> IO State
run = foldM step state0

test = run testInput

testInput = [
    "value 5 goes to bot 2",
    "bot 2 gives low to bot 1 and high to bot 0",
    "value 3 goes to bot 1",
    "bot 1 gives low to output 1 and high to bot 0",
    "bot 0 gives low to output 2 and high to output 0",
    "value 2 goes to bot 2"
  ]

-- test = run state0 testInput

main = do input <- readFile "input.txt"
          state <- run (lines input)
          let v = product $ map (_outputs state Map.!) [0, 1, 2]
          print v
