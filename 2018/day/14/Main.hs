import Control.Applicative
import Data.Char
import Data.List (tails, findIndex, isPrefixOf)
import Data.Rope (Rope)
import qualified Data.Rope as Rope  -- https://github.com/gwils/rope/tree/new-ghcs

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
        fmt k x | k == i = '(':x:')':[]
        fmt k x | k == j = '[':x:']':[]
        fmt k x          = ' ':x:' ':[]

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

    -- Part 2
    let cond (State _ sb) =
            let t = show input
                -- step produces (at most) two new recipes; test both
                xs = take 2 $ tails $ Rope.toString $ Rope.drop (Rope.length sb - length t - 1) sb
            in any (t `isPrefixOf`) xs
    let ans (State _ sb) =
            -- Subtract 1 if the scoreboard does not end with
            -- the input (in which case the last char should be
            -- removed)
            let t = show input
                s = Rope.toString $ Rope.drop (Rope.length sb - length t) sb
            in Rope.length sb - length t - if t == s then 0 else 1

    print $ ans $ until cond step s0
