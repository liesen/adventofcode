import Control.Applicative
import Data.Char
import Data.Maybe
import Text.ParserCombinators.ReadP
import Z3.Monad

data Nanobot = Nanobot
    { position :: (Int, Int, Int)
    , radius :: Int
    } deriving (Eq, Show)

parseNanobot :: ReadP Nanobot
parseNanobot = do
    pos <- string "pos=" *> between (char '<') (char '>') (liftA3 (,,) num (char ',' *> num) (char ',' *> num))
    _ <- string ", "
    r <- string "r=" *> num
    return (Nanobot pos r)
  where
    num = read <$> munch1 (\x -> x == '-' || isDigit x)

parse = endBy parseNanobot (char '\n') <* eof

bounds nanobots = ((xmin, xmax), (ymin, ymax), (zmin, zmax))
  where
    xmin = minimum (map (\Nanobot{position=(x, y, z), radius=r} -> x - r) nanobots)
    xmax = maximum (map (\Nanobot{position=(x, y, z), radius=r} -> x + r) nanobots)
    ymin = minimum (map (\Nanobot{position=(x, y, z), radius=r} -> y - r) nanobots)
    ymax = maximum (map (\Nanobot{position=(x, y, z), radius=r} -> y + r) nanobots)
    zmin = minimum (map (\Nanobot{position=(x, y, z), radius=r} -> z - r) nanobots)
    zmax = maximum (map (\Nanobot{position=(x, y, z), radius=r} -> z + r) nanobots)

search_z3 :: [Nanobot] -> Z3 Integer
search_z3 nanobots = do
    -- Z3 literals
    _0 <- mkInteger 0
    _1 <- mkInteger 1

    -- Coordinates of the optimal location
    x <- mkFreshIntVar "x"
    y <- mkFreshIntVar "y"
    z <- mkFreshIntVar "z"

    -- Make sure we are inside reasonable bounds (maybe it helps with performance?)
    let ((xmin, xmax), (ymin, ymax), (zmin, zmax)) = Main.bounds nanobots
    optimizeAssert =<< mkGe x =<< mkIntNum xmin
    optimizeAssert =<< mkLe x =<< mkIntNum xmax
    optimizeAssert =<< mkGe y =<< mkIntNum ymin
    optimizeAssert =<< mkLe y =<< mkIntNum ymax
    optimizeAssert =<< mkGe z =<< mkIntNum zmin
    optimizeAssert =<< mkLe z =<< mkIntNum zmax

    -- Absolute value of a Z3 number
    let z3abs x = do
         cond <- mkGe x _0
         x' <- mkUnaryMinus x
         mkIte cond x x'

    -- Distance of location to a position
    let z3distance (x', y', z') = do
         dx <- z3abs =<< mkSub [x, x']
         dy <- z3abs =<< mkSub [y, y']
         dz <- z3abs =<< mkSub [z, z']
         mkAdd [dx, dy, dz]

    let inRange (Nanobot {position=(nx, ny, nz), radius=nr}) = do
         nx' <- mkIntNum nx
         ny' <- mkIntNum ny
         nz' <- mkIntNum nz
         nr' <- mkIntNum nr
         d' <- z3distance (nx', ny', nz')
         cond <- mkLe d' nr'
         mkIte cond _1 _0

    -- Maximize number of nanobots in range of location
    optimizeMaximize =<< mkAdd =<< mapM inRange nanobots

    -- Minimize distance of location to origin, also the answer
    ans <- mkFreshIntVar "ans"
    optimizeAssert =<< mkEq ans =<< z3distance (_0, _0, _0)
    optimizeMinimize ans

    Sat <- optimizeCheck []
    model <- optimizeGetModel
    fromJust <$> evalInt model ans

main = do
    input <- readFile "input.txt"
    let [(nanobots, "")] = readP_to_S parse input

    -- Part 2
    print =<< evalZ3 (search_z3 nanobots)
