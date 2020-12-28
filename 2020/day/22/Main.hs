{-# LANGUAGE ImportQualifiedPost #-}
import Data.Char
import Data.List
import Data.Ord
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Sequence (Seq(..))
import Data.Sequence qualified as Seq
import Text.ParserCombinators.ReadP
import Debug.Trace

type Deck = Seq Int

player :: ReadP (Int, Deck)
player = (,) <$> (string "Player " *> decimal <* string ":\n")
             <*> (Seq.fromList <$> decimal `endBy` char '\n')

parse = player `sepBy` char '\n'

decimal = read <$> munch1 isDigit

combat :: Deck -> Deck -> (Int, Deck)
combat Empty            winner           = (2, winner)
combat winner           Empty            = (1, winner)
combat (top1 :<| deck1) (top2 :<| deck2)
    | top1 >= top2 = combat (deck1 :|> top1 :|> top2) deck2
    | otherwise    = combat (deck1) (deck2 :|> top2 :|> top1)

score deck = fst $ foldr (\card (acc, d) -> (acc + card * d, d + 1)) (0, 1) deck

recursiveCombat player1 player2 = go mempty player1 player2
  where
    go seen Empty                    player2 = (2, player2)
    go seen player1                  Empty   = (1, player2)
    go seen player1@(top1 :<| deck1) player2@(top2 :<| deck2)
        | Set.member player1 seen || Set.member player2 seen = (1, player1)
        | top1 <= length deck1 && top2 <= length deck2 =
            case recursiveCombat (Seq.take top1 deck1) (Seq.take top2 deck2) of
              (1, _) -> go seen' (deck1 :|> top1 :|> top2) deck2
              (2, _) -> go seen' deck1 (deck2 :|> top2 :|> top1)
        | top1 >= top2 = go seen' (deck1 :|> top1 :|> top2) deck2
        | otherwise    = go seen' deck1 (deck2 :|> top2 :|> top1)
      where
        seen' = Set.insert player1 (Set.insert player2 seen)

main = do
    input <- readFile "input.txt"
    let [(((1, player1), (2, player2)), "")] = readP_to_S ((,) <$> player <*> (char '\n' *> player) <* eof) input

    -- Part 1
    print $ score . snd $ combat player1 player2

    print $ score . snd $ recursiveCombat player1 player2

    -- 34039 too high
    -- 20048 too low