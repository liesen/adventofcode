import Data.Char
import Data.List
import Text.ParserCombinators.ReadP
import Control.Monad
import Data.Monoid
import qualified Data.Set as Set
import Debug.Trace
import Data.Maybe

data Cond
    = Range Int Int
    | Or Cond Cond
    deriving (Eq, Ord, Show)

eval (Range lo hi) n = lo <= n && hi >= n
eval (Or a b)      n = eval a n || eval b n

data Rule = Rule String Cond
    deriving (Show)

-- range :: ReadP Rule
range = Range <$> decimal <*> (char '-' *> decimal)

decimal :: ReadP Int
decimal = read <$> munch1 isDigit

cond = Or <$> range <*> (string " or " *> range)

rule = Rule <$> munch (/= ':') <*> (string ": " *> cond)

newtype Ticket = Ticket [Int] deriving (Show)

ticket = Ticket <$> decimal `sepBy` char ','

parse :: ReadP ([Rule], Ticket, [Ticket])
parse = do
    rules <- rule `endBy` char '\n'
    string "\nyour ticket:\n"
    yourTicket <- ticket <* char '\n'
    string "\nnearby tickets:\n"
    nearbyTickets <- ticket `endBy` char '\n'
    return (rules, yourTicket, nearbyTickets)

-- test (ruleName, cond) (Ticket x) = eval cond x
validate rules (Ticket xs) = and (zipWith f rules xs)
  where
    f (Rule _ cond) = eval cond

completelyInvalid rules (Ticket xs) =
    any (\rule -> not (any (f rule) xs)) rules
  where
    f (Rule _ cond) = eval cond

part1 :: [Rule] -> Ticket -> Int
part1 rules (Ticket xs) = sum $ mapMaybe invalid xs
  where
    invalid x
        | not (any (\(Rule _ cond) -> eval cond x) rules) = Just x
        | otherwise = Nothing

validIndices (Rule _ cond) (Ticket xs) = [i | (i, x) <- zip [0..] xs, eval cond x]

shrink :: [(Rule, [Int])] -> [(Int, Rule)]
shrink = go mempty . sortOn (length . snd)
  where
    go seen [] = []
    go seen ((rule, xs):ys) =
        case filter (`notElem` seen) xs of
            [x] -> (x, rule) : go (insert x seen) ys
            _   -> error "cannot determine order of rules"

main = do
    input <- readFile "input.txt"
    let [((rules, myTicket, nearbyTickets), "")] = readP_to_S (parse <* eof) input

    -- Part 1
    print $ getSum $ foldMap (Sum . part1 rules) nearbyTickets

    -- Part 2
    let validTickets = filter ((== 0) . part1 rules) nearbyTickets
        candidates = [(rule, foldr1 intersect (map (validIndices rule) validTickets)) | rule <- rules]
        fields = shrink candidates
        Ticket values = myTicket 

    print $ product [values !! i | (i, Rule name _) <- fields, "departure" `isPrefixOf` name]