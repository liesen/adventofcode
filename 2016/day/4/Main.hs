{-# LANGUAGE RecordWildCards #-}
--- Day 4: Security Through Obscurity ---
import Control.Arrow ((&&&))
import Data.Char
import Data.List
import Data.Maybe
import Data.Ord

data Room = Room { _name :: String, _sector :: Int, _checksum :: String }
  deriving (Show)

parse :: String -> Room
parse = parse' . reverse
  where
    -- Found it easier to parse the string reversed: the
    -- "structure" is at the end of the string
    parse' xs =
      let (checksum, xs') = checksum' xs
          (sector, xs'') = sector' xs'
      in Room { _checksum = checksum, _sector = sector, _name = reverse xs'' }
    checksum' (']':xs) =
      let (checksum, '[':xs') = break (== '[') xs
      in (reverse checksum, xs')
    sector' xs =
      let (sector, xs') = span isDigit xs
      in (read (reverse sector), xs')

checksum = map fst . take 5 . sortBy (comparing (negate . snd)) . map (head &&& length) . group . sort . filter (/= '-') . _name

real = uncurry (==) . (checksum &&& _checksum)

run1 = sum . map _sector . filter real . map parse . lines

main1 = readFile "input.txt" >>= print . run1

--- Part Two ---
decrypt Room{..} = map (rotate _sector) _name
  where
    rotate _ '-' = ' '
    rotate n x = chr ((ord x - ord 'a' + n) `mod` 26 + ord 'a')

run2 = _sector . fromJust . find f . filter real . map parse . lines
  where
    f = isPrefixOf "northpole" . decrypt

main2 = readFile "input.txt" >>= print . run2

main = main1 >> main2
