import Text.ParserCombinators.ReadP

-- (Cube) coordinate in a (flat top) hexagonal grid:
-- https://www.redblobgames.com/grids/hexagons/#coordinates-cube
data P = P (Int, Int, Int) deriving (Eq, Show)

instance Monoid P where
  mempty = P (0, 0, 0)
  mappend (P (x0, y0, z0)) (P (x1, y1, z1)) = P (x0 + x1, y0 + y1, z0 + z1)
  
parse = readP_to_S (sepBy (choice [n, ne, se, s, sw, nw]) (char ',') <* eof)
  where
    n = string "n" >> return (P (0, 1, -1))
    ne = string "ne" >> return (P (1, 0, -1))
    nw = string "nw" >> return (P (-1, 1, 0))
    s = string "s" >> return (P (0, -1, 1))
    se = string "se" >> return (P (1, -1, 0))
    sw = string "sw" >> return (P (-1, 0, 1))

distance (P (x0, y0, z0)) (P (x1, y1, z1)) = maximum [abs (x0 - x1), abs (y0 - y1), abs (z0 - z1)]

main = do
    input <- readFile "input.txt"
    let path = fst . head . parse . head . lines $ input
    print $ distance mempty $ mconcat path
    print $ maximum . map (distance mempty) $ scanl mappend mempty path
