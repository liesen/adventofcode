import Data.Maybe

-- 1
type X = (Int, Int)

n :: X -> Int
n (x, y) = y * 3 + x + 1

type F = X -> Maybe X

type G = X -> X

u, d, l, r :: F
u (x, y) = if y == 0 then Nothing else Just (x, y - 1)
d (x, y) = if y == 2 then Nothing else Just (x, y + 1)
l (x, y) = if x == 0 then Nothing else Just (x - 1, y)
r (x, y) = if x == 2 then Nothing else Just (x + 1, y)

clamp :: F -> G
clamp f x = fromMaybe x (f x)

code :: X -> [F] -> X
code = foldl (\x f -> (clamp f) x)

parse 'U' = u
parse 'D' = d
parse 'L' = l
parse 'R' = r

test = "ULL\nRRDDD\nLURDL\nUUUUD"

program :: String -> [Int]
program = tail . map n . scanl code (1, 1) . map (map parse) . lines

-- 2
-- U D L R
move '1' 'D' = Just '3'

move '2' 'D' = Just '6'
move '2' 'R' = Just '3'

move '3' 'U' = Just '1'
move '3' 'D' = Just '7'
move '3' 'L' = Just '2'
move '3' 'R' = Just '4'

move '4' 'D' = Just '8'
move '4' 'L' = Just '3'

move '5' 'R' = Just '6'

move '6' 'U' = Just '2'
move '6' 'D' = Just 'A'
move '6' 'L' = Just '5'
move '6' 'R' = Just '7'

move '7' 'U' = Just '3'
move '7' 'D' = Just 'B'
move '7' 'L' = Just '6'
move '7' 'R' = Just '8'

move '8' 'U' = Just '4'
move '8' 'D' = Just 'C'
move '8' 'L' = Just '7'
move '8' 'R' = Just '9'

move '9' 'L' = Just '8'

move 'A' 'U' = Just '6'
move 'A' 'R' = Just 'B'

move 'B' 'U' = Just '7'
move 'B' 'D' = Just 'D'
move 'B' 'L' = Just 'A'
move 'B' 'R' = Just 'C'

move 'C' 'U' = Just '8'
move 'C' 'L' = Just 'B'

move 'D' 'U' = Just 'B'

move _   _ = Nothing

program' :: String -> [Char]
program' = tail . scanl (foldl (\x i -> fromMaybe x (move x i))) '5' . lines

main = readFile "input.txt" >>= putStrLn . program'
