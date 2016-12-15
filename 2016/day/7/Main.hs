{-# LANGUAGE ViewPatterns #-}
import Data.List
import Data.Either

parse "" = []
parse xs =
    case break (== '[') xs of
      (a, '[':xs') ->
        case break (== ']') xs' of
          (b, ']':xs'') -> Left a : Right b : parse xs''
          (b, _) -> [Left a, Right b]
      (a, _) -> [Left a]

abba :: String -> Bool
abba = any (\[a, b, c, d] -> a == d && b == c && a /= b && c /= d) . filter ((== 4) . length) . map (take 4) . tails

abbaS = either (Left . abba) (Right . abba)

tls (partitionEithers . map abbaS . parse -> (s, hs)) = or s && not (or hs)

test1 s = (s, tls s)

tests1 = [
    test1 "abba[mnop]qrst",
    test1 "abcd[bddb]xyyx",
    test1 "aaaa[qwer]tyui",
    test1 "ioxxoj[asdfgh]zxcvbn"
  ]

main1 = readFile "input.txt" >>= print . length . filter tls . lines

--- Part Two ---
aba :: [Char] -> Bool
aba [a, b, c] = a == c && a /= b
aba _         = False

abas = filter aba . filter ((== 3) . length) . map (take 3) . tails

aba2bab [a, b, c] = [b, a, b]

ssl (partitionEithers . parse -> (s, hs)) = not . null $ concatMap (map aba2bab . abas) s `intersect` concatMap abas hs

test2 s = (s, ssl s)

tests2 = [
    test2 "aba[bab]xyz",
    test2 "xyx[xyx]xyx",
    test2 "aaa[kek]eke",
    test2 "zazbz[bzb]cdb"
  ]

-- 231
main2 = readFile "input.txt" >>= print . length . filter ssl . lines

main = main1 >> main2
