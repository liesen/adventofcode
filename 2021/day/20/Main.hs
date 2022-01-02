{-# LANGUAGE ImportQualifiedPost, BinaryLiterals, ViewPatterns #-}
import Data.Array (range)
import Data.Bits
import Data.Set (Set)
import Data.Set qualified as Set
import Data.List (elemIndices)

type Algorithm = Set Int

data Image = Image
    { bg :: Bool  -- True if the background is dark; false otherwise
    , pixels :: Set (Int, Int)  -- All pixels in the opposite color to the bg
    }

pixelToBool '.' = False
pixelToBool '#' = True

boolToPixel False = '.'
boolToPixel True = '#'

bounds im = ((minrow, mincol), (maxrow, maxcol))
    where
        rows = Set.map fst im
        cols = Set.map snd im
        minrow = minimum rows
        mincol = minimum cols
        maxrow = maximum rows
        maxcol = maximum cols

pp (Image bg im)
    = unlines [ [boolToPixel (Set.member (r, c) im /= bg) | c <- [mincol..maxcol]]
              | r <- [minrow..maxrow]
              ]
      ++
      show (length im)
    where
        ((minrow, mincol), (maxrow, maxcol)) = bounds im

parse :: String -> (Algorithm, Image)
parse (break (== '\n') -> (alg', span (== '\n') -> (_, pixels')))
    = (alg, Image bg pixels)
  where
    alg = Set.fromList $ elemIndices '#' alg'
    bg = False  -- Initial image is always dark bg
    pixels = Set.fromList [ (r, c)
                          | (r, ln) <- zip [0..] (lines pixels')
                          , (c, '#') <- zip [0..] ln
                          ]

step :: Algorithm -> Image -> Image
step alg (Image bg im) = Image bg' im'
  where
    -- Background will flip between light and dark
    bg' = Set.member (if bg then 0b111111111 else 0b000000000) alg
    im' = Set.filter update (Set.fromList (concatMap neighbors im))
    update p = Set.member ix alg /= bg'
      where
        ix = sum [ if Set.member p' im /= bg then 1 `shiftL` i else 0 
                 | (p', i) <- zip (neighbors p) [8,7..]
                 ]

neighbors (r, c) = do
    r' <- [r - 1..r + 1]
    c' <- [c - 1..c + 1]
    return (r', c')

main = do
    input <- readFile "input.txt"
    let (alg, im) = parse input
        steps = iterate (step alg) im

    -- Part 1
    print $ length $ pixels $ steps !! 2

    -- Part 2
    print $ length $ pixels $ steps !! 50

