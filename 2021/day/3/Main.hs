import Control.Arrow ((***))
import Data.Char (digitToInt)
import Data.List
import Data.Monoid

toDecimal :: String -> Int
toDecimal = foldl' (\acc x -> acc * 2 + digitToInt x) 0

countDigits :: [String] -> [(Int, Int)]
countDigits numbers = map ((length *** length) . partition (== '0')) (transpose numbers)

gammaRate = map f . countDigits
  where f (n0, n1) | n0 > n1   = '0'
                   | otherwise = '1'

diagnosticReport :: (Int -> Int -> Bool) -> [String] -> String
diagnosticReport bitCriteria numbers = head $ snd $ until done sel (0, numbers)
  where
    done (_, [x]) = True  -- Found it
    done _        = False

    sel (i, xs) =
        let (xs0, xs1) = partition ((== '0') . (!! i)) xs
        in if bitCriteria (length xs0) (length xs1)
               then (i + 1, xs1)
               else (i + 1, xs0)

main = do
    input <- readFile "input.txt"
    let numbers = lines input

    -- Part 1
    let gamma = gammaRate numbers
        epsilon = map f gamma  -- Can simply invert the gamma rate
          where f '0' = '1'
                f '1' = '0'
    print $ toDecimal gamma * toDecimal epsilon

    -- Part 2
    let oxygen = diagnosticReport (<=) numbers
        co2 = diagnosticReport (>) numbers
    print $ toDecimal oxygen * toDecimal co2
