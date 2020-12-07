import Control.Monad
import Data.Array (inRange)
import Data.Char
import Data.List
import Text.ParserCombinators.ReadP

type Field = (String, String)

type Passport = [Field]

parseField :: ReadP Field
parseField = (,) <$> many (satisfy isAlpha) <* char ':' <*> manyTill (satisfy (not . isSpace)) (satisfy isSpace)

parse :: ReadP [Passport]
parse = many parseField `sepBy` char '\n'

complete :: Passport -> Bool
complete x = sort (delete "cid" (map fst x)) == ["byr", "ecl", "eyr", "hcl", "hgt", "iyr", "pid"]

validRange :: String
              -> Passport
              -> Int
              -> (Int, Int)
              -> Bool
validRange field fields len range = maybe False test (lookup field fields)
    where
        test value = length value == len && all isDigit value && inRange range (read value)

validHeight = maybe False test . lookup "hgt"
    where
        test value =
            let (n, unit) = span isDigit value
            in (unit == "cm" && inRange (150, 193) (read n)) ||
                (unit == "in" && inRange (59, 76) (read n))

validHex ('#':s) = length s == 6 && all (\c -> inRange ('0', '9') c || inRange ('a', 'f') c) s
validHex _ = False

validHairColor = maybe False validHex . lookup "hcl"

validEyeColor = maybe False test . lookup "ecl"
    where
        test value = value `elem` ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]

validPassportId = maybe False test . lookup "pid"
    where
        test value = length value == 9 && all isDigit value

valid :: Passport -> Bool
valid x = and
    [ validRange "byr" x 4 (1920, 2002)
    , validRange "iyr" x 4 (2010, 2020)
    , validRange "eyr" x 4 (2020, 2030)
    , validHeight x 
    , validHairColor x
    , validEyeColor x
    , validPassportId x
    ]

main = do
    input <- readFile "input.txt"
    let [(passports, "")] = readP_to_S (parse <* eof) input

    -- Part 1
    let (xs, _) = partition complete passports
    print $ length xs

    -- Part 2
    print $ length $ filter valid xs
