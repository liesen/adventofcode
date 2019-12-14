{-# LANGUAGE RecordWildCards #-}
import Control.Applicative
import Control.Monad
import Data.Char
import Data.List
import Data.Map (Map)
import Data.Ratio
import Debug.Trace
import qualified Data.Map as Map
import Text.ParserCombinators.ReadP


data Item = Item { substance :: String, num :: Integer }

instance Show Item where
    show Item{..} = show num ++ " " ++ substance

scale a (Item s num) = Item s (num * a)

data Reaction = Reaction { reagents :: [Item], yield :: Item }

instance Show Reaction where
    show Reaction{..} = intercalate " + " (map show reagents) ++ " => " ++ show yield

parseItem = flip Item <$> (read <$> munch1 isDigit) <*> (skipSpaces *> munch1 isAlpha)

parseReaction = Reaction <$> sepBy1 parseItem (string ", ") <*> (string " => " *> parseItem)

parseFile = many1 (parseReaction <* char '\n') <* eof

eval :: [Reaction] -> Map String Integer -> Item -> (Integer, Map String Integer)
eval reactions carry (Item "ORE" n) = (n, carry)
eval reactions carry (Item x 0) = (0, carry)
eval reactions carry (Item x n) =
    -- Want to get n items of substance x
    case Map.lookup x carry of
        -- Found some x that was already produced
        Just m
            | m <= n    -> eval reactions (Map.delete x carry) (Item x (n - m))
            | otherwise -> (0, Map.insert x (m - n) carry)
        Nothing ->
            -- Find reaction that produces x
            let Just reaction@(Reaction reagents (Item _ m)) = find ((x ==) . substance . yield) reactions
                -- Reaction produces m of x, and we want n; find the scaling factor, q, so that: m * q >= n
                q = ceiling (n % m)
                r = q * m - n  -- Number of extra x produced
                carry' = Map.insertWith (+) x r carry
                reagents' = map (scale q) reagents  -- Scale formula to get enough x
            -- Find cost to produce all the (scaled) reagents
            -- in trace ("Use " ++ show reaction ++ " to consume " ++ intercalate ", " (map show reagents') ++ " to produce " ++ show (Item x n) ++ " with rest: " ++ show (Item x r)) $ 
            in foldl (\(a, carry') item -> let (b, carry'') = eval reactions carry' item in (a + b, carry'')) (0, carry') reagents'

test file = do
    input <- readFile file
    let [(reactions, "")] = readP_to_S parseFile input
    mapM_ print reactions
    putStrLn (replicate 20 '-')
    print $ eval reactions mempty (Item "FUEL" 1)

test1 = test "testinput1.txt"
test2 = test "testinput2.txt"
test3 = test "testinput3.txt"
test4 = test "testinput4.txt"

search f 0        ans = ans
search f stepSize guess =
    let (lo, hi) = (f (guess - stepSize), f (guess + stepSize))
    in case (compare lo 1000000000000, compare hi 1000000000000) of
        (LT, LT) -> search f stepSize (guess + stepSize)
        (GT, GT) -> search f stepSize (guess - stepSize)
        (LT, GT) -> search f (stepSize `div` 2) guess

main = do
    input <- readFile "input.txt"
    let [(reactions, "")] = readP_to_S parseFile input

    -- Part 1
    print $ fst $ eval reactions mempty (Item "FUEL" 1)

    -- Part 2
    print $ search (fst . eval reactions mempty . Item "FUEL") 100000 100000
