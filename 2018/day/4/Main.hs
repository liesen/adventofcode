{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}
import Control.Monad
import Data.Array
import Data.Char
import Data.List
import qualified Data.Map as Map
import Data.Monoid
import Data.Ord
import Data.Time
import Text.ParserCombinators.ReadP
import Text.Printf


data Action = BeginsShift Int | FallsAsleep | WakesUp deriving (Eq)

instance Show Action where
    show (BeginsShift i) = printf "Guard #%d begins shift" i
    show FallsAsleep = "falls asleep"
    show WakesUp = "wakes up"


data Record = Record { timestamp :: LocalTime, action :: Action } deriving (Eq)

instance Ord Record where
    compare = comparing timestamp

instance Show Record where
    show (Record (LocalTime d (TimeOfDay h m s)) a) = 
        "[" ++ (showGregorian d) ++ " " ++ (printf "%02d:%02d" h m) ++ "] " ++ (show a)


parseTimestamp :: ReadP LocalTime
parseTimestamp = do
    between (char '[') (char ']') $ do
        y <- number 4
        char '-'
        m <- number 2
        char '-'
        d <- number 2
        char ' '
        hh <- number 2
        char ':'
        mm <- number 2
        return $ LocalTime (fromGregorian y m d) (TimeOfDay hh mm 0)
  where
    number n = read `fmap` count n (satisfy isDigit)

parseRecord :: ReadP Record
parseRecord = do
    t <- parseTimestamp
    char ' '
    a <- parseBeginsShift +++ parseFallsAsleep +++ parseWakesUp
    return $ Record t a
  where 
    number = read <$> many1 (satisfy isDigit)
    parseBeginsShift = BeginsShift <$> (string "Guard #" *> number <* string " begins shift")
    parseFallsAsleep = pure FallsAsleep <* string "falls asleep"
    parseWakesUp = pure WakesUp <* string "wakes up"

parse :: ReadP [Record]
parse = sepBy parseRecord (char '\n') <* optional (char '\n') <* eof


-- Applies a strategy to each shift
strategy :: Monoid a => ([Record] -> a) -> [Record] -> [(Int, a)]
strategy f = unfoldr shift
  where
    shift [] = Nothing
    shift (x@(Record _ (BeginsShift i)):(break beginsShift -> (xs, ys))) = Just ((i, f (x:xs)), ys)
    beginsShift = \case
        (Record _ (BeginsShift _)) -> True
        _ -> False


-- Track minutes slept strategy
newtype Sleep = Sleep (Array Int Int) 

instance Monoid Sleep where
    mempty = Sleep (listArray (0, 59) (replicate 60 0))
    mappend (Sleep a) (Sleep b) = Sleep (accum (+) a (assocs b))

instance Show Sleep where
    show (Sleep a) = map (\case 0 -> '.'; i -> intToDigit i) (elems a)

asleep :: [Record] -> Sleep
asleep records = mconcat $ zipWith f records (tail records)
  where
    f (Record t0 FallsAsleep) (Record t1 WakesUp) = Sleep $ accumArray (+) 0 (0, 59) $ map (\t -> (t, 1)) $ minutes t1 t0
    f (Record t0 (BeginsShift i)) (Record t1 FallsAsleep) = mempty
    f (Record t0 WakesUp) (Record t1 FallsAsleep) = mempty

minutes (LocalTime ad (TimeOfDay _ am _)) (LocalTime bd (TimeOfDay _ bm _)) = map (`mod` 60) [bm..(fromIntegral (diffDays ad bd) * 60) + am - 1]


-- Date & time 
diffLocalTime a b = diffUTCTime (localTimeToUTC utc a) (localTimeToUTC utc b)

diffLocalTimeMinutes a b = truncate (diffLocalTime a b) `div` 60


main = do
    input <- readFile "input.txt"
    let [(records, "")] = readP_to_S parse input
        strategize f =
            let (guardId, Sleep s) = maximumBy (comparing (f . snd)) $ Map.assocs $ Map.fromListWith mappend $ strategy asleep $ sort records
                (m, _) = maximumBy (comparing snd) (assocs s)
            in guardId * m

    -- Part 1
    print $ strategize sleepTime

    -- Part 2
    print $ strategize sleepCount
  where
    sleepTime (Sleep a) = sum (elems a)
    sleepCount (Sleep a) = maximum (elems a)
