import qualified Data.Sequence as Seq
import Data.Sequence (Seq (..))

--- Day 19: An Elephant Named Joseph ---
input = 3005290

play1 n = go (Seq.fromList [1..n])
  where
    go (x :<| Empty)      = x
    go (x :<| (y :<| zs)) = go (zs <> Seq.singleton x)

play2 n = go (Seq.fromList [1..n])
  where
    go (x :<| Empty) = x
    go xs            = let (y :<| ys, z :<| zs) = Seq.splitAt (Seq.length xs `div` 2) xs
                       in go (ys <> zs <> Seq.singleton y)

main = do
    -- Part 1
    print $ play1 input

    -- Part 2
    print $ play2 input
