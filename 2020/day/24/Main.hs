{-# LANGUAGE ImportQualifiedPost #-}
import Text.ParserCombinators.ReadP
import Data.List
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Map (Map)
import Data.Map qualified as Map

data HexCoord = HexCoord !Int !Int !Int deriving (Eq, Ord, Show)

instance Semigroup HexCoord where
    (HexCoord x1 y1 z1) <> (HexCoord x2 y2 z2) = HexCoord (x1 + x2) (y1 + y2) (z1 + z2)

instance Monoid HexCoord where
    mempty = HexCoord 0 0 0

e = HexCoord 1 (-1) 0
se = HexCoord 0 (-1) 1
sw = HexCoord (-1) 0 1
w = HexCoord (-1) 1 0
nw = HexCoord 0 1 (-1)
ne = HexCoord 1 0 (-1)

parseCoord []           = Nothing
parseCoord ('e':xs)     = Just (e, xs)
parseCoord ('s':'e':xs) = Just (se, xs)
parseCoord ('s':'w':xs) = Just (sw, xs)
parseCoord ('w':xs)     = Just (w, xs)
parseCoord ('n':'w':xs) = Just (nw, xs)
parseCoord ('n':'e':xs) = Just (ne, xs)

parseLine = mconcat . unfoldr parseCoord

paint black []     = black
paint black (p:ps)
    | p `Set.member` black = paint (Set.delete p black) ps
    | otherwise            = paint (Set.insert p black) ps

neighbors p = Set.fromList $ map (p <>) [e, se, sw, w, nw, ne]

step black = Set.filter keep candidates
  where
    candidates = black <> foldMap neighbors black
    keep p
        | isBlack && (k == 0 || k > 2) = False 
        | not isBlack && k == 2        = True
        | otherwise                    = isBlack
      where
        isBlack = p `Set.member` black
        k = length (Set.filter (`Set.member` black) (neighbors p))

main = do
    input <- readFile "input.txt"
    
    -- Part 1
    let black = paint mempty $ map parseLine $ lines input
    print $ length black

    -- Part 2
    print $ length $ iterate step black !! 100
