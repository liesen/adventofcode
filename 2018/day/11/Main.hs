import Control.Concurrent
import Control.Monad
import Data.Array
import Data.List
import Data.Ord

powerLevel serialNo (x, y) = (((rackId * y + serialNo) * rackId) `div` 100) `mod` 10 - 5
    where rackId = x + 10

powerLevels serialNo = listArray ((1, 1), (300, 300)) [powerLevel serialNo (x, y) | x <- [1..300], y <- [1..300]]

maxPowerLevel serialNo size =
    maximumBy (comparing snd) [((x, y, size), n) | x <- [1..300 - size], y <- [1..300 - size], let n = sum (map (a !) (range ((x, y), (x + size - 1, y + size - 1))))]
  where
    a = powerLevels serialNo

-- Lazily produces all tuples, (x, y, size), and its sum
maxPowerLevel2 :: Int -> [((Int, Int, Int), Int)]
maxPowerLevel2 serialNo =
    [((x, y, size), n) | size <- [3..300], x <- [1..300 - size], y <- [1..300 - size], let n = sum (map (a !) (range ((x, y), (x + size - 1, y + size - 1))))]
  where
    a = powerLevels serialNo

main = do
    let serialNo = 4172

    -- Part 1
    let ((x, y, _), _) = maxPowerLevel serialNo 3
    putStrLn $ show x ++ "," ++ show y

    -- Part 2
    -- Find the tuple with the maximum sum produced in one minute
    mstop <- newEmptyMVar
    mvar <- newMVar (maxPowerLevel2 serialNo)
    let loop = do
        stop <- tryReadMVar mstop
        case stop of
            Just _ -> return ()
            Nothing -> modifyMVar_ mvar (\(x:y:xs) -> return $! if snd y > snd x then y:xs else x:xs) >> loop
    threadId <- forkIO loop
    threadDelay (60 * 10^6)
    putMVar mstop ()
    killThread threadId
    ((x, y, size), _) <- head <$> takeMVar mvar
    putStrLn $ show x ++ "," ++ show y ++ "," ++ show size
