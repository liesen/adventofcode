import Control.Applicative
import Control.Monad (unless, zipWithM)
import Data.Char
import Data.List
import Text.ParserCombinators.ReadP

parse :: ReadP (Integer, [Maybe Integer])
parse = (,) <$> number <* char '\n' <*> schedule <* char '\n'

number = read <$> munch1 isDigit

busId = Nothing <$ char 'x' <|> Just <$> number

schedule = busId `sepBy` char ','

-- https://rosettacode.org/wiki/Chinese_remainder_theorem#Haskell
egcd :: Integer -> Integer -> (Integer, Integer)
egcd _ 0 = (1, 0)
egcd a b = (t, s - q * t)
  where
    (s, t) = egcd b r
    (q, r) = a `quotRem` b
 
modInv :: Integer -> Integer -> Either String Integer
modInv a b =
  case egcd a b of
    (x, y)
      | a * x + b * y == 1 -> Right x
      | otherwise ->
        Left $ "No modular inverse for " ++ show a ++ " and " ++ show b
 
chineseRemainder :: [Integer] -> [Integer] -> Either String Integer
chineseRemainder residues modulii =
  zipWithM modInv crtModulii modulii >>=
  (Right . (`mod` modPI) . sum . zipWith (*) crtModulii . zipWith (*) residues)
  where
    modPI = product modulii
    crtModulii = (modPI `div`) <$> modulii

-- Time to wait from time t if the bus leaves every dt units:
--     (t - dt) % dt
--   = -t % dt
ttw t dt = (-t) `mod` dt

main = do
    input <- readFile "input.txt"
    let [((t, busIds), "")] = readP_to_S parse input

    -- Part 1
    print $ uncurry (*) $ minimum [(ttw t b, b) | Just b <- busIds]

    -- Part 2
    let (residues, modulii) = unzip [(ttw i b, b) | (i, Just b) <- zip [0..] busIds]
        Right ans2 = chineseRemainder residues modulii
    print ans2
