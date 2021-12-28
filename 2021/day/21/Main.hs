{-# LANGUAGE ImportQualifiedPost, ViewPatterns #-}
import Data.List
import Data.Map.Strict qualified as Map
import Data.Monoid
import Debug.Trace

type Die = [[Int]] -- All possible outcomes at each step

data Player = Player
    { position :: Int
    , score :: Int
    }
    deriving (Eq, Show)

data State = State
    { die :: Die
    , numRolls :: Int
    , numTurns :: Int
    , activePlayer :: Player
    , otherPlayer :: Player
    }
    deriving (Eq)

deterministicDie = go (cycle [1..100])
  where
    go (d1:d2:d3:ds) = [d1 + d2 + d3]:go ds

quantumDie = repeat [d1 + d2 + d3 | d1 <- [1..3], d2 <- [1..3], d3 <- [1..3]]

instance Show State where
    show (State die numRolls numTurns player1 player2) =
        "State {numRolls = " ++ show numRolls
            ++ ", player1 = " ++ show player1
            ++ ", player2 = " ++ show player2
            ++ "}"

halfTurn (State ([roll]:die') numRolls numTurns p1@(Player pos1 score1) p2)
    = State die' (numRolls + 3) (numTurns + 1) p2 p1'
  where
    pos1' = (pos1 - 1 + roll) `mod` 10 + 1
    score1' = score1 + pos1'
    p1' = Player pos1' score1'

done winningScore (State _ _ _ (Player _ score1) (Player _ score2))
    = score1 >= winningScore || score2 >= winningScore

game winningScore = until (done winningScore) halfTurn

parse1 (stripPrefix "Player 1 starting position: " -> Just pos) = Player (read pos) 0
parse2 (stripPrefix "Player 2 starting position: " -> Just pos) = Player (read pos) 0

countWins winningScore = fst . go winningScore mempty
  where
    go winningScore memo state@(State die numRolls numTurns p1@(Player pos1 score1) p2@(Player pos2 score2))
        | score1 >= winningScore = ((Sum 1, Sum 0), memo)
        | score2 >= winningScore = ((Sum 0, Sum 1), memo)
        | Just wins <- Map.lookup key memo = (wins, memo)
        | otherwise =
            let ((wins2, wins1), memo') = foldl f ((Sum 0, Sum 0), memo) (halfTurns state)
                wins = (wins1, wins2)
            in (wins, Map.insert key wins memo')
       where
         key = (pos1, score1, pos2, score2)
         f (wins, memo) state = let (wins', memo') = go winningScore memo state in (wins <> wins', memo')

-- Generates all possible half turns
halfTurns (State (rolls:die') numRolls numTurns p1@(Player pos1 score1) p2)
    = do
        roll <- rolls
        let pos1' = (pos1 - 1 + roll) `mod` 10 + 1
            score1' = score1 + pos1'
            p1' = Player pos1' score1'
        return (State die' (numRolls + 3) (numTurns + 1) p2 p1')

main = do
    input <- readFile "input.txt"
    let [(parse1 -> p1), (parse2 -> p2)] = lines input

    -- Part 1
    let (State _ numRolls _ (Player _ score1) (Player _ score2))
            = game 1000 (State deterministicDie 0 0 p1 p2)
    print $ numRolls * min score1 score2

    -- Part 2
    let (wins1, wins2) = countWins 21 (State quantumDie 0 0 p1 p2)
    print $ getSum $ max wins1 wins2
