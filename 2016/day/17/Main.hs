import Control.Monad
import qualified Data.ByteString.Char8 as C
import Crypto.Hash
import Crypto.Hash.Algorithms (MD5)

import Data.List (partition, maximumBy)
import Data.Ord

open :: Char -> Bool
open = flip elem "bcdef"

type Pos = (Int, Int)

type State = (Pos, Context MD5)

initialState s = ((0, 0), hashUpdate hashInit (C.pack s))

-- up, down, left, right
udlr = zip [(0, -1), (0, 1), (-1, 0), (1, 0)] "UDLR"

trans (pos@(x, y), ctx) = do
    ((dx, dy), ch) <- udlr'
    let pos'@(x', y') = (x + dx, y + dy)
    guard $ x' >= 0 && x' <= 3 && y' >= 0 && y' <= 3
    let ctx' = hashUpdate ctx (C.singleton ch)
    return $ (ch, (pos', ctx'))
  where
    udlr' = map snd $ filter (open . fst) $ zip hash udlr
    hash = show (hashFinalize ctx)

isSolution (str, (pos, ctx)) = pos == (3, 3)

space f s = expand f (step ([], s))
  where
    expand f []     = []
    expand f (s:ss) = s : expand f (f (step s) ss)
    step (ms, s)    = [(ms ++ [m], t) | (m, t) <- trans s]

solutions f = filter isSolution . space f

bfs = flip (++)

asdf = solutions bfs $ initialState "yjjvjgan"

solution1 :: (String, (Pos, Context MD5))
solution1 = head asdf 

main1 = putStrLn (fst solution1)

--- Part Two ---

-- The /space/ function in part one does not stop when it is at
-- a solution... but, why?
space2 s = expand (step ([], s))
  where
    expand []  = []
    expand ss  = let (xs, ss') = partition isSolution ss in xs ++ expand (concatMap step ss')
    step (ms, s) = [(ms ++ [m], t) | (m, t) <- trans s]

solution2 :: (String, (Pos, Context MD5))
solution2 = maximumBy (comparing (length . fst)) solutions
  where
    solutions = space2 (initialState "yjjvjgan")

main2 = print (length (fst solution2))

main = main1 >> main2
