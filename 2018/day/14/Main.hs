import Data.Char
import Data.List (tails, findIndex, isPrefixOf)
import Data.Rope (Rope)
import qualified Data.Rope as Rope  -- https://github.com/gwils/rope/tree/new-ghcs
import Text.Printf

data State = State
    { elves :: [Int]
    , scoreboard :: Rope
    }

instance Show State where
    show (State elves scoreboard) =
        let [i, j] = elves
        in concat $ zipWith fmt [0..] (Rope.toString scoreboard)
      where
        [i, j] = elves
        fmt k x | k == i = printf "(%c)" x
        fmt k x | k == j = printf "[%c]" x
        fmt k x          = printf " %c " x

step (State elves scoreboard) =
    let scores = map (digitToInt . Rope.head . flip Rope.drop scoreboard) elves
        n = sum scores
        scoreboard' = scoreboard `mappend` Rope.pack (show n)
        elves' = zipWith (\elf score -> (elf + score + 1) `mod` Rope.length scoreboard') elves scores
    in State elves' scoreboard'

s0 = State [0,1] (Rope.pack "37")

main = do
    let input = 990941

    -- Part 1
    let cond (State _ sb) = Rope.length sb > input + 10
    putStrLn $ Rope.toString . Rope.take 10 . Rope.drop input . scoreboard $ until cond step s0
