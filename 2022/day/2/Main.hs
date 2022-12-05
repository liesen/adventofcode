import Control.Arrow
import Data.Char
import Data.Monoid


data Shape
    = Rock
    | Paper
    | Scissor
    deriving (Eq, Ord, Show)


shapeScore Rock = 1
shapeScore Paper = 2
shapeScore Scissor = 3

data Round = Round Shape Shape

decode :: Char -> Shape
decode 'A' = Rock
decode 'B' = Paper
decode 'C' = Scissor
decode 'X' = Rock
decode 'Y' = Paper
decode 'Z' = Scissor

win Rock = Paper
win Paper = Scissor
win Scissor = Rock

lose Rock = Scissor
lose Paper = Rock
lose Scissor = Paper

strategy1 = score

strategy2 (Round p1 p2) =
    case p2 of
        Rock -> score (Round p1 (lose p1))
        Paper -> score (Round p1 p1)
        Scissor -> score (Round p1 (win p1))

score (Round p1 p2) = shapeScore p2 + outcomeScore
  where
    outcomeScore
      | win p1 == p2 = 6
      | p1 == p2 = 3
      | otherwise = 0

parseRound :: MonadFail m => String -> m Round
parseRound [p, ' ', q] = pure (Round (decode p) (decode q))
parseRound _           = fail "bad input"

main = do
    input <- readFile "input.txt"
    rounds <- traverse parseRound (lines input)

    -- Part 1
    print $ getSum $ foldMap (Sum . strategy1) rounds

    -- Part 2
    print $ getSum $ foldMap (Sum . strategy2) rounds
