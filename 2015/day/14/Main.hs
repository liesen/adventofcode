{-# LANGUAGE ViewPatterns, RecordWildCards #-}

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

main = do
    input <- readFile "input.txt"
    let reindeers = map parse $ lines input

    -- Part 1
    print $ maximum $ map (race 2503) reindeers