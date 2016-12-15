import Control.Applicative
import Data.Char

data Turn = L | R deriving (Show, Eq)
data Dir = N | W | E | S deriving (Show, Eq)

newtype I = I (Turn, Int) deriving (Show, Eq)
newtype X = X (Dir, (Int, Int)) deriving (Show, Eq)

dir' N L = W
dir' N R = E
dir' W L = S
dir' W R = N
dir' E L = N
dir' E R = S
dir' S L = E
dir' S R = W

pInstruction :: String -> I 
pInstruction ('L':xs) = I (L, read xs)
pInstruction ('R':xs) = I (R, read xs)

splitComma s =
    case break (== ',') s of
      (x, ',':' ':xs) -> x : splitComma xs
      (x, [])         -> [x]
      _ -> []

input = (map pInstruction . splitComma) <$> readFile "input.txt"

move (x, y) N n = (x, y + n)
move (x, y) W n = (x - n, y)
move (x, y) E n = (x + n, y)
move (x, y) S n = (x, y - n)

move' (x, y) N n = [(x, y + i) | i <- [1..n]]
move' (x, y) W n = [(x - i, y) | i <- [1..n]] 
move' (x, y) E n = [(x + i, y) | i <- [1..n]]
move' (x, y) S n = [(x, y - i) | i <- [1..n]]

step (X (dir, (x, y))) (I (lr, n)) = let d = dir' dir lr in X (d, move (x, y) d n)
step' (X (dir, (x, y))) (I (lr, n)) = let d = dir' dir lr in map (\p -> X (d, p)) (move' (x, y) d n)

x0 = X (N, (0, 0))

run = foldl step (X (N, (0, 0)))

dist (X (_dir, (x, y))) = abs x + abs y

tests = map (dist . run) [[I (L, 2), I (R, 3)], replicate 3 (I (R, 2)), [I (R, 5), I (L, 5), I (R, 5), I (R, 3)]]

-- 291
main = input >>= print . dist . run

twice :: [I] -> Maybe X
twice is = go x0 [] is []
  where
    go x             (y@(X (_dir, xy)):ys) is seen
      | xy `elem` seen = Just y
      | otherwise     = go x ys is (xy : seen)
    go x@(X (_, xy)) []     []     seen = Nothing
    go x@(X (_, xy)) []     (i:is) seen = go (step x i) (step' x i) is (xy : seen)

-- 159
main2 = input >>= print . maybe "nothing" (show . dist) . twice
