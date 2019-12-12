{-# LANGUAGE RecordWildCards #-}
import Data.List
import qualified Data.Map.Strict as Map

type V3 = [Int]  -- [x, y, z]

data Moon = Moon { pos :: V3, vel :: V3 } deriving (Eq)

instance Show Moon where
    show (Moon pos vel) = "pos=<" ++ showVec pos ++ ">, vel=<" ++ showVec vel ++ ">"
        where xyz = ["x=", "y=", "z="]
              showVec = intercalate ", " . zipWith (++) xyz . map show

pot = sum . map abs . pos

kin = sum . map abs . vel

problem = [ Moon { pos = [1, -4, 3], vel = [0, 0, 0] }
          , Moon { pos = [-14, 9, -4], vel = [0, 0, 0] }
          , Moon { pos = [-4, -6, 7], vel = [0, 0, 0] }
          , Moon { pos = [6, -9, -11], vel = [0, 0, 0] }
          ]

example1 = [ Moon { pos = [-1, 0, 2], vel = [0, 0, 0] }
           , Moon { pos = [2, -10, -7], vel = [0, 0, 0] }
           , Moon { pos = [4, -8, 8], vel = [0, 0, 0] }
           , Moon { pos = [3, 5, -1], vel = [0, 0, 0] }
           ]

example2 = [ Moon { pos = [-8, -10, 0], vel = [0, 0, 0] }
           , Moon { pos = [5, 5, 10], vel = [0, 0, 0] }
           , Moon { pos = [2, -7, 3], vel = [0, 0, 0] }
           , Moon { pos = [9, -8, -3], vel = [0, 0, 0] }
           ]

step1 :: [Moon] -> [Moon]
step1 ms = [ Moon pos' vel'
           | m <- ms
           , let dvel = map sum $ transpose [map signum (zipWith (-) (pos n) (pos m)) | n <- ms]
           , let vel' = zipWith (+) (vel m) dvel
           , let pos' = zipWith (+) (pos m) vel'
           ]

energy :: [Moon] -> Int
energy = sum . map (\m -> pot m * kin m)

xpos, ypos, zpos, xvel, yvel, zvel :: [Int]
xpos = [1, -14, -4, 6]  -- map ((!! 0) . pos) problem
ypos = [-4, 9, -6, -9]
zpos = [3, -4, 7, -11]
xvel = [0, 0, 0, 0]
yvel = [0, 0, 0, 0]
zvel = [0, 0, 0, 0]

-- Find next set of x, y or z positions and velocities
step2 :: ([Int], [Int]) -> ([Int], [Int])
step2 (pos, vel) =
    let dvel = [sum [signum (b - a) | b <- pos] | a <- pos]
        vel' = zipWith (+) vel dvel
    in (zipWith (+) pos vel', vel')

findCycleLength :: ([Int], [Int]) -> Integer
findCycleLength = go 0 mempty
  where
    go t m pv =
        case Map.lookup pv m of
            Nothing -> go (t + 1) (Map.insert pv t m) (step2 pv)
            Just t0 -> t - t0

cycleLengths = map findCycleLength [(xpos, yvel), (ypos, yvel), (zpos, zvel)]

main = do
    -- Part 1
    print $ energy $ iterate step1 problem !! 1000

    -- Part 2
    -- Find the cycle in each ordinate separately, then find
    -- the lowest common multiple of the three
    print $ foldl1 lcm cycleLengths