import Control.Applicative
import Data.List
import Data.Set (Set)
import qualified Data.Set as Set
import Text.ParserCombinators.ReadP
import Text.Printf

data Pots = Pots
    { pots :: String
    , index :: Int
    , rules :: [(String, Char)]
    } deriving (Eq, Ord, Show)

parse = do
    s0 <- string "initial state: " *> manyTill get (char '\n')
    _ <- char '\n'
    rules <- sepBy (liftA3 (\r _ y -> (r, y)) (count 5 get) (string " => ") get) (char '\n')
    _ <- char '\n'
    eof
    return $ Pots s0 0 rules

showPots (Pots s i _) = 
    let (xs, y:ys) = splitAt (-i) s
    in "(" ++ show i ++ ") " ++ reverse (take 3 (reverse xs)) ++ "[" ++ [y] ++ "]" ++ ys

score (Pots s i _) =
    sum $ map fst $ filter ((== '#') . snd) $ zip [i..] s

step (Pots s i rules) =
    let s' = map go $ map (take 5) $ filter ((>= 5) . length) $ tails ("....." ++ s ++ ".....")
    in Pots s' (i - 3) rules
  where
    go xs = case lookup xs rules of
                Nothing -> '.'
                Just y  -> y

clean (Pots s i rules) =
    let s' = reverse (dropWhile (== '.') (reverse s))
        a = length (takeWhile (== '.') s')
    in Pots (drop a s') (i + a) rules

main = do
    input <- readFile "input.txt"
    let [(s0, "")] = readP_to_S parse input

    -- Part 1
    print $ score $ iterate step s0 !! 20
