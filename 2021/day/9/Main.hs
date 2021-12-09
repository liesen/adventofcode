import Data.Char
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe

nb (x, y) = [(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)]

lowPoints heightmap = Map.filterWithKey lowPoint heightmap
  where
    lowPoint p h = all (> h) (mapMaybe (`Map.lookup` heightmap) (nb p))

main = do
    input <- readFile "input.txt"

    let heightmap :: Map (Int, Int) Int
        heightmap = Map.fromList $ do
            (r, row) <- zip [0..] (lines input)
            (c, col) <- zip [0..] row 
            return ((r, c), digitToInt col)

    print $ sum $ fmap (+ 1) $ lowPoints heightmap
