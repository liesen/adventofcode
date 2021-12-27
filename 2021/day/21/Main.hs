{-# LANGUAGE ViewPatterns #-}
import Debug.Trace
import Data.List (stripPrefix)


data Player = Player
    { position :: Int
    , score :: Int
    }
    deriving (Eq, Show)

data State = State
    { die :: [Int]
    , numRolls :: Int
    , numTurns :: Int
    , player1 :: Player
    , player2 :: Player
    }
    deriving (Eq)

initState die p1 p2 = State die 0 0 p1 p2

instance Show State where
    show (State die numRolls numTurns player1 player2) = 
        "State {numRolls = " ++ show numRolls 
            ++ ", player1 = " ++ show player1
            ++ ", player2 = " ++ show player2
            ++ "}"

example = initState (cycle [1..100]) (Player 4 0) (Player 8 0)

halfTurn (State die numRolls numTurns p1@(Player pos1 score1) p2@(Player pos2 score2))
    | even numTurns = State die' (numRolls + 3) (numTurns + 1) p1' p2 
    | otherwise     = State die' (numRolls + 3) (numTurns + 1) p1 p2'
  where
    (roll, die') = splitAt 3 die

    pos1' = wrap (pos1 + sum roll)
    score1' = score1 + pos1'
    p1' = Player pos1' score1'

    pos2' = wrap (pos2 + sum roll)
    score2' = score2 + pos2'
    p2' = Player pos2' score2'

wrap p
    | p <= 10   = p
    | otherwise = wrap (p - 10)

done winningScore (State _ _ _ (Player _ score1) (Player _ score2))
    = score1 >= winningScore || score2 >= winningScore

game winningScore = until (done winningScore) halfTurn

parse1 (stripPrefix "Player 1 starting position: " -> Just pos) = Player (read pos) 0
parse2 (stripPrefix "Player 2 starting position: " -> Just pos) = Player (read pos) 0

main = do
    input <- readFile "input.txt"
    let [(parse1 -> p1), (parse2 -> p2)] = lines input
        deterministicDie = cycle [1..100]

    -- Part 1
    let (State _ numRolls _ (Player _ score1) (Player _ score2))
            = game 1000 (initState deterministicDie p1 p2)
    print $ numRolls * min score1 score2
