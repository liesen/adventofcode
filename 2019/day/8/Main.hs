import Data.Array
import Data.Ord
import Data.Char
import Data.List
import Data.List.Split
import Data.Monoid

data Pixel = Black | White | Trans deriving Show

-- Enum instance for pixel
toPixel '0' = Black
toPixel '1' = White
toPixel '2' = Trans

fromPixel Black = '0'
fromPixel White = '1'
fromPixel Trans = '2'

instance Monoid Pixel where
    mempty = Trans
    Trans `mappend` x = x
    x `mappend` Trans = x
    x `mappend` y     = x

newtype Layer = Layer (Array (Int, Int) Pixel) deriving Show

instance Monoid Layer where
    mempty = error "undefined"
    Layer a `mappend` Layer b = Layer $ array (bounds a) $ map (\i -> (i, a ! i <> b ! i)) (range (bounds a))

newtype Image = Image [Layer]

decodeLayer :: Int -> Int -> [Char] -> Maybe (Layer, [Char])
decodeLayer w h [] = Nothing
decodeLayer w h xs = Just (Layer (listArray ((0, 0), (w - 1, h - 1)) (map toPixel xs)), drop n xs)
  where
    n = w * h

decodeImage w h = Image . unfoldr (decodeLayer w h)

encode :: Image -> [Char]
encode (Image layers) = let Layer a = foldl1 (<>) layers
                        in map fromPixel (elems a)

main = do
    input <- head . lines <$> readFile "input.txt"
    let (width, height) = (25, 6)

    -- Part 1
    let layer0 = minimumBy (comparing (length . takeWhile (== '0') . sort)) (chunksOf (width * height) input)
    print $ length (filter (== '1') layer0) * length (filter (== '2') layer0)

    -- Part 2
    let image@(Image layers) = decodeImage width height input
    putStr $ unlines $ chunksOf width
           $ map (\x -> if x == '1' then '@' else ' ') $ encode image
