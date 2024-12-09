{-# LANGUAGE ViewPatterns #-}

import Data.Char (digitToInt, intToDigit)
import Data.Sequence (Seq (..), (<|), (|>))
import Data.Sequence qualified as Seq

data Block
  = Space Int
  | File Int {- FileId -} Int {- File size -} Bool {- Has moved flag used in part 2 -}
  deriving (Eq, Ord, Show)

parse :: String -> Seq Block
parse = Seq.fromList . zipWith block [0 ..]
  where
    block i (digitToInt -> size)
      | isFile = File ino size False
      | otherwise = Space size
      where
        isFile = i `mod` 2 == 0
        ino = i `div` 2

unparse :: Seq Block -> String
unparse = concatMap block
  where
    block (Space free) = replicate free '.'
    block (File ino size _) = replicate size (intToDigit ino)

-- Part 1: pop files from the back, fill any free space at the front
compact1 :: Seq Block -> Seq Block
compact1 = go Seq.empty
  where
    go done Empty = done
    go done (f@(File ino size False) :<| todo) = go (done |> File ino size True) todo
    go done (Space free :<| (todo :|> Space _)) = go done (Space free <| todo) -- Remove all free space to the right (we don't need it for calculating the checksum)
    go done (Space free :<| (todo :|> File ino size False)) -- Don't really need to update the moved flag but do it anyway
      | size < free = go (done |> File ino size True) (Space (free - size) <| todo)
      | size == free = go (done |> File ino size True) todo
      | otherwise = go (done |> File ino free True) (todo |> File ino (size - free) False)

-- Part 2: pop files from the back, put only where there's enough free space
-- to fit the whole file
compact2 :: Seq Block -> Seq Block
compact2 Empty = Seq.empty
compact2 (xs :|> s@(Space _)) = compact2 xs |> s
compact2 (xs :|> f@(File _ _ True)) = compact2 xs |> f -- Already moved
compact2 (xs :|> f@(File ino size False)) =
  case Seq.breakl (fits f) xs of
    -- No space to fit file
    (xs', Empty) -> compact2 xs' |> f'
    -- Found space for file
    (xs', Space free :<| ys) ->
      if size < free
        then compact2 ((xs' |> f') <> (Space (free - size) <| (ys |> Space size)))
        else compact2 ((xs' |> f') <> (ys |> Space size))
  where
    fits (File _ size _) (Space free) = size <= free
    fits _ _ = False

    -- File marked as moved
    f' = File ino size True

checksum = fst . foldl block (0, 0)
  where
    block (ans, i) (Space size) = (ans, i + size)
    block (ans, i) (File ino size _mv) = (ans + sum (map (ino *) [i .. i + size - 1]), i + size)

main = do
  input <- readFile "input"
  let diskMap = parse input -- "2333133121414131402"

  -- Part 1
  print $ checksum $ compact1 diskMap

  -- Part 2
  print $ checksum $ compact2 diskMap