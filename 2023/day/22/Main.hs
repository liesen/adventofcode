import Control.Monad
import Data.Array
import Data.Char
import Data.List
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe
import Data.Ord
import Data.Set (Set)
import Data.Set qualified as Set
import Text.ParserCombinators.ReadP

type V = (Int, Int, Int)

x, y, z :: V -> Int
x (x1, y1, z1) = x1
y (x1, y1, z1) = y1
z (x1, y1, z1) = z1

type Brick = (V, V)

xx, yy, zz :: Brick -> (Int, Int)
xx (b1, b2) = (x b1, x b2)
yy (b1, b2) = (y b1, y b2)
zz (b1, b2) = (z b1, z b2)

example1 :: [Brick]
example1 =
  [ ((1, 0, 1), (1, 2, 1)),
    ((0, 0, 2), (2, 0, 2)),
    ((0, 2, 3), (2, 2, 3)),
    ((0, 0, 4), (0, 2, 4)),
    ((2, 0, 5), (2, 2, 5)),
    ((0, 1, 6), (2, 1, 6)),
    ((1, 1, 8), (1, 1, 9))
  ]

drop :: Set V -> Brick -> Either Brick Brick
drop bricks brick@((x1, y1, z1), (x2, y2, z2))
  | zmin' == 0 = Left brick -- Below ground
  | any (`Set.member` bricks) (range brick') = Left brick -- Hit another brick
  | otherwise = Right brick' -- Continue falling
  where
    zmin' = min z1 z2 - 1
    brick' = ((x1, y1, z1 - 1), (x2, y2, z2 - 1))

snapshot :: [(V, V)] -> String
snapshot bricks = unlines lines
  where
    xzMap =
      Map.fromList
        [ ((x, z), i) | (i, bnds) <- zip ['A' ..] bricks, (x, y, z) <- range bnds
        ]
    yzMap =
      Map.fromList
        [ ((y, z), i) | (i, bnds) <- zip ['A' ..] bricks, (x, y, z) <- range bnds
        ]
    xmax = maximum $ map (\((x1, y1, z1), (x2, y2, z2)) -> max x1 x2) bricks
    ymax = maximum $ map (\((x1, y1, z1), (x2, y2, z2)) -> max y1 y2) bricks
    zmax = maximum $ map (\((x1, y1, z1), (x2, y2, z2)) -> max z1 z2) bricks
    xz =
      [ [fromMaybe '.' (Map.lookup (x, z) xzMap) | x <- [0 .. xmax]] | z <- [zmax, zmax - 1 .. 1]
      ]
    yz =
      [ [fromMaybe '.' (Map.lookup (y, z) yzMap) | y <- [0 .. ymax]] | z <- [zmax, zmax - 1 .. 1]
      ]
    -- header = unwords (replicate 2
    lines =
      zipWith3
        (\xz yz zi -> unwords [xz, yz, show zi])
        (xz ++ [ground])
        (yz ++ [ground])
        [zmax, zmax - 1 .. 0]
    ground = replicate (xmax + 1) '-'
    xzLines = ["x/z", map intToDigit [0 .. min xmax 9]] ++ xz ++ [ground]

settle bricks =
  let ((modified, settled), bricks') = mapAccumL go (False, mempty) $ sortBy (comparing (uncurry min . zz)) bricks
   in (modified, bricks')
  where
    go (modified, settled) brick =
      case Main.drop settled brick of
        Left brick' -> ((modified, Set.union settled (Set.fromList (range brick'))), brick')
        Right brick' -> go (True, settled) brick'

parse = endBy1 parseBrick (char '\n') <* eof
  where
    parseBrick = (,) <$> parseV <*> (char '~' *> parseV)
    parseV = (,,) <$> parseNum <*> (char ',' *> parseNum) <*> (char ',' *> parseNum)
    parseNum = read <$> many1 (satisfy isDigit)

main = do
  input <- readFile "input"
  let [(bricks, "")] = readP_to_S parse input

  -- Settle all bricks
  let (_, bricks') = settle bricks

  -- Part 1
  print $ length $ filter (\brick -> not (fst (settle (bricks' \\ [brick])))) bricks'
