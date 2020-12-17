import Control.Monad
import Data.Set (Set)
import qualified Data.Set as Set

data Vec3 = Vec3 !Int !Int !Int
    deriving (Eq, Ord, Show)

neighbors3 (Vec3 x y z) = do
    dx <- d
    dy <- d
    dz <- d
    guard $ not (dx == 0 && dy == 0 && dz == 0)
    return $ Vec3 (x + dx) (y + dy) (z + dz)
  where
    d = [-1, 0, 1]

data Vec4 = Vec4 !Int !Int !Int !Int
    deriving (Eq, Ord, Show)

neighbors4 (Vec4 x y z w) = do
    dx <- d
    dy <- d
    dz <- d
    dw <- d
    guard $ not (dx == 0 && dy == 0 && dz == 0 && dw == 0)
    return $ Vec4 (x + dx) (y + dy) (z + dz) (w + dw)
  where
    d = [-1, 0, 1]

step :: Ord a => (a -> Set a) -> Set a -> Set a
step neighbors active = Set.filter keep candidates
    where
        candidates = active `Set.union` foldMap neighbors active
        keep p
            | Set.member p active = k == 2 || k == 3
            | otherwise           = k == 3
            where
                countActive = length . Set.filter (`Set.member` active)
                k = countActive (neighbors p)

parse :: String -> [(Int, Int)]
parse input = [ (x, y) | (y, ys) <- zip [0..] (lines input), (x, '#') <- zip [0..] ys]

vec3 (x, y) = Vec3 x y 0
vec4 (x, y) = Vec4 x y 0 0

main = do
    input <- readFile "input.txt"
    let active = parse input

    -- Part 1
    print $ length $ (!! 6) $ iterate (step (Set.fromList . neighbors3)) $ Set.fromList $ map vec3 active

    -- Part 2
    print $ length $ (!! 6) $ iterate (step (Set.fromList . neighbors4)) $ Set.fromList $ map vec4 active
