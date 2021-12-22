import Data.Char
import Data.Tree
import Debug.Trace
import Text.ParserCombinators.ReadP

-- Snail number
data S
    = R Int  -- Regular number
    | P S S  -- Pair
    deriving (Show, Eq, Ord)

pp :: S -> String
pp (R x)   = show x
pp (P a b) = "[" ++ pp a ++ "," ++ pp b ++ "]"

-- Useful to represent as a Tree to get levels and showTree
toTree (R x)   = Node (Just x) []
toTree (P a b) = Node Nothing [toTree a, toTree b]

ppTree = drawTree . fmap (maybe "" show) . toTree

-- Parsing
number :: ReadP S
number = regular +++ pair
  where
    pair = between (char '[') (char ']') (P <$> number <*> (char (',') *> number))
    regular = R . read <$> munch1 isDigit

parse = fst . head . readP_to_S (number <* eof)

-- Add left
(<+) :: Int -> S -> S
x <+ R y   = R (x + y)
x <+ P a b = P (x <+ a) b

-- Add right
(+>) :: S -> Int -> S
R y +> x   = R (y + x)
P a b +> x = P a (b +> x)

-- Debris from an explosion
data Debris
    = AddLR Int Int
    | AddL Int
    | AddR Int
    | Done
    deriving (Show)

explode :: S -> Maybe S
explode = fmap snd . go 0
  where
    go :: Int -> S -> Maybe (Debris, S)
    go _ n@(R _) = Nothing
    go d n@(P (R x) (R y)) | d == 4 = Just (AddLR x y, R 0)
    go d n@(P a b) =
        case go (d + 1) a of 
          Just (AddLR x y, a') -> Just (AddL x, P a' (y <+ b))
          Just (AddL x, a')    -> Just (AddL x, P a' b)
          Just (AddR y, a')    -> Just (Done, P a' (y <+ b))
          Just (Done, a')      -> Just (Done, P a' b)
          Nothing ->
            case go (d + 1) b of
              Just (AddLR x y, b') -> Just (AddR y, P (a +> x) b')
              Just (AddL x, b')    -> Just (Done, P (a +> x) b')
              Just (AddR y, b')    -> Just (AddR y, P a b')
              Just (Done, b')      -> Just (Done, P a b')
              Nothing              -> Nothing

split :: S -> Maybe S
split (R x)
    | x >= 10    = Just (P (R (x `div` 2)) (R (x - x `div` 2)))
    | otherwise = Nothing
split (P a b)   =
    case split a of
        Just a' -> Just (P a' b)
        Nothing -> P a <$> split b

-- Reduction
data Reduction a
    = StepExplode a
    | StepSplit a
    | StepDone a
    deriving (Eq, Ord, Show)

reduce :: S -> S
reduce = stepExplode . StepExplode
  where
    stepExplode prev@(StepExplode n) =
        case explode n of
            Nothing -> stepSplit (StepExplode n)
            Just n' ->
                -- trace ("after explode: " ++ pp n') $
                stepExplode (StepExplode n')
    stepExplode prev@(StepSplit n) =
        case explode n of
            Nothing -> stepSplit (StepExplode n)
            Just n' ->
                -- trace ("after explode: " ++ pp n') $
                stepExplode (StepExplode n')

    stepSplit (StepExplode n) =
        case split n of
            Nothing -> n
            Just n' ->
                -- trace ("after split: " ++ pp n') $
                stepExplode (StepSplit n')
    stepSplit (StepSplit n) =
        case split n of
            Nothing -> stepExplode (StepSplit n)
            Just n' ->
                -- trace ("after split: " ++ pp n') $
                stepExplode (StepSplit n')

-- Addition
instance Semigroup S where
    a <> b = reduce (P a b)

magnitude (R x)   = x
magnitude (P a b) = 3 * magnitude a + 2 * magnitude b

main = do
    input <- readFile "input.txt"
    let numbers = map parse $ lines input
    
    -- Part 1
    print $ magnitude $ foldl1 (<>) (numbers)

    -- Part 2
    print $ maximum [ magnitude (a <> b) | a <- numbers, b <- numbers ]
