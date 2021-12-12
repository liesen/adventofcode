import Data.Char
import Data.List
import Data.Set (Set)
import qualified Data.Set as Set

type Cave = String
type Caves = [(Cave, Cave)]

count1 :: Caves -> Set Cave -> [Cave] -> Integer
count1 caves seen []        = 0
count1 caves seen ("end":_) = 1
count1 caves seen (x:q1)
    | Set.member x seen = count1 caves seen q1
    | all isUpper x     = sum $ map (count1 caves seen . (:q1)) q2
    | all isLower x     = sum $ map (count1 caves (Set.insert x seen) . (:q1)) q2
    where
        q2 = neighbors caves x

count2 :: Caves -> Set Cave -> Maybe Cave -> [Cave] -> Integer
count2 caves seen1 _       []         = 0
count2 caves seen1 _       ("end":xs) = 1
count2 caves seen1 Nothing (x:q1)  -- Able to double-visit any small cave
    | Set.member x seen1 = sum $ map (count2 caves seen1 (Just x) . (:q1)) q2  -- Double-visit x
    | all isUpper x      = sum $ map (count2 caves seen1 Nothing . (:q1)) q2
    | all isLower x      = sum $ map (count2 caves (Set.insert x seen1) Nothing . (:q1)) q2
    where
        q2 = neighbors caves x
count2 caves seen1 seen2   (x:q1)  -- Already double-visited some cave
    | Set.member x seen1 = count2 caves seen1 seen2 q1
    | all isUpper x      = sum $ map (count2 caves seen1 seen2 . (:q1)) q2
    | all isLower x      = sum $ map (count2 caves (Set.insert x seen1) seen2 . (:q1)) q2
    where
        q2 = neighbors caves x

parseLine x = let (src, '-':dst) = break (== '-') x in [(src, dst), (dst, src)]

neighbors caves "end" = []
neighbors caves p     = [dst | (src, dst) <- caves, src == p, dst /= "start"]

main = do
    input <- readFile "input.txt"
    let caves = concatMap parseLine (lines input)

    -- Part 1
    print $ count1 caves mempty ["start"]

    -- Part 2
    print $ count2 caves mempty Nothing ["start"]

