--- Day 5: How About a Nice Game of Chess? ---
import Control.Arrow ((&&&))
import qualified Data.ByteString.Char8 as C
import Crypto.Hash
import Crypto.Hash.Algorithms (MD5)
import Data.Array
import Data.Char
import Data.List
import Data.Maybe
import Data.Monoid
import System.Environment (getArgs)
import Debug.Trace

interesting = (== "00000") . take 5 . show

hashes :: String -> [Digest MD5]
hashes doorId =
    filter interesting $ map md5 [0..]
  where
    ctx :: Context MD5
    ctx = hashUpdate hashInit (C.pack doorId)

    md5 = hashFinalize . hashUpdate ctx . C.pack . show

search1 = take 8 . map char . hashes
  where
    char = (!! 5) . show

main1 = getArgs >>= putStrLn . search1 . head

--- Part Two ---
unfold2 :: String -> [(Int, Char)]
unfold2 = map (\(x, y) -> (digitToInt x, y)) . filter valid . map ix . hashes
  where
    ix = ((!! 5) &&& (!! 6)) . show
    valid = flip elem ['0'..'7'] . fst

fold2 :: [(Int, Char)] -> String
fold2 = go (listArray (0, 7) (replicate 8 (First Nothing)))
  where
    go xs [] = map (fromJust . getFirst) (elems xs)
    go xs ((i, x):ixs)
      | all (isJust . getFirst) (elems xs) = map (fromJust . getFirst) (elems xs)
      | otherwise = 
          let x' = xs ! i `mappend` First (Just x)
              xs' = xs // [(i, x')]
          in go xs' ixs

search2 = fold2 . unfold2

input = "ffykfhsq"

main = do putStrLn (search1 input)
          putStrLn (search2 input)
          
