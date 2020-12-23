{-# LANGUAGE ImportQualifiedPost #-}
import Data.Char
import Data.Foldable
import Data.Sequence (Seq(..), (<|), (|>))
import Data.Sequence qualified as Seq

input = "219748365"

newtype Cups = Cups { getLabels :: Seq Int }

instance Show Cups where
    show (Cups cups) = let (xs, _ :<| ys) = Seq.breakl (== 1) cups
                       in map intToDigit (toList (ys <> xs))

step (Cups (curr :<| a :<| b :<| c :<| rest)) =
    let (xs, _dest :<| ys) = Seq.breakl (== dest) (curr <| rest)
        (xs', _curr :<| next :<| ys') = Seq.breakl (== curr) (xs <> (dest <| a <| b <| c <| ys))
    in Cups $ (next <| ys') <> (xs' |> curr)
  where
    dest = let k = until (`notElem` [a, b, c]) pred (curr - 1)
           in if k `elem` rest then k else maximum rest

main = do
    let labels = Seq.fromList (map digitToInt testinput)

    -- Part 1
    print $ iterate step (Cups labels) !! 100
