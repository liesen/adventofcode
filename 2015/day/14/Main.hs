{-# LANGUAGE ViewPatterns, RecordWildCards #-}
import Data.List

data Reindeer = Reindeer
    { name :: String
    , speed :: Int
    , time :: Int
    , rest :: Int
    }
  deriving (Eq, Show)

-- Dancer can fly 27 km/s for 5 seconds, but then must rest for 132 seconds.
parse (words -> [name, "can", "fly", read -> speed, "km/s", "for", read -> time, "seconds,", "but", "then", "must", "rest", "for", read -> rest, "seconds."]) = Reindeer{..}

data State
    = Flew  -- Is this correct English?
    | Rested
  deriving Show

type Time = Int

type Distance = Int

type Intermediate = (Time, Distance, State)

steps :: Reindeer -> [Intermediate]
steps Reindeer{..} = iterate step (0, 0, Rested)
  where
    step (t, dist, Rested) = (t + time, dist + speed * time, Flew)
    step (t, dist, Flew)  = (t + rest, dist, Rested)

adjust :: Reindeer -> Time -> Intermediate -> Distance
adjust Reindeer{..} t (t', dist, Flew)   = dist
adjust Reindeer{..} t (t', dist, Rested) = dist + speed * (t - t')

race :: Time -> Reindeer -> Distance
race t reindeer = adjust reindeer t $ last $ takeWhile (\(t', _, _) -> t' <= t) $ steps reindeer

ticks = interpolate . steps
  where
    interpolate ((t0, dist0, state0):(s@(t1, dist1, state1)):ss) =
      [(t0 + dt, dist0 + dd, state0) | dt <- [0..t1 - t0 - 1], let dd = dt * ((dist1 - dist0) `div` (t1 - t0))] ++ interpolate (s:ss)

type Points = Int

rewardLeader :: [(Time, Distance, State, Points)] -> [(Time, Distance, State, Points)]
rewardLeader xs =
    let maxDist = maximum $ map (\(t, dist, _, _) -> dist) xs
    in map (\(t, dist, state, points) -> (t, dist, state, points + if dist == maxDist then 1 else 0)) xs

main = do
    input <- readFile "input.txt"
    let reindeers = map parse $ lines input

    -- Part 1
    print $ maximum $ map (race 2503) reindeers

    -- Part 2
    print $ maximum
          $ map (\(t, d, s, p) -> p)
          $ map (foldl1 (\(t1, d1, s1, p1) (t2, d2, s2, p2) -> (t2, d2, s2, p1 + p2)))
          $ transpose
          $ map rewardLeader
          $ transpose
          $ map (map (\(t, dist, state) -> (t, dist, state, 0)))
          $ map (take 2503 . ticks) reindeers