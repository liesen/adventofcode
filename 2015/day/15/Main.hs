{-# LANGUAGE RecordWildCards #-}
import Data.Char
import Data.List
import Data.Ord
import Text.ParserCombinators.ReadP

data Ingredient = Ingredient
    { name :: String
    , capacity :: Int
    , durability :: Int
    , flavor :: Int 
    , texture :: Int
    , calories :: Int
    } deriving (Eq, Show)

parseIngredient = do
    name <- (:) <$> satisfy isUpper <*> munch isLower
    _ <- string ": "
    capacity <- string "capacity " *> number
    _ <- string ", "
    durability <- string "durability " *> number
    _ <- string ", "
    flavor <- string "flavor " *> number
    _ <- string ", "
    texture <- string "texture " *> number
    _ <- string ", "
    calories <- string "calories " *> number
    return Ingredient{..}
  where
    number = do
        op <- option '+' (char '-')
        n <- read <$> munch1 isDigit
        return $ if op == '-' then -n else n

bake 0 _      = fail "not enough ingredients"
bake n [i]    = [[(n, i)]]
bake n (i:is) = [(m, i):ms | m <- [0..n], ms <- bake (n - m) is]

score = product . map (max 0 . sum) . transpose . map (\(n, Ingredient{..}) -> map (n *) [capacity, durability, flavor, texture])

main = do
    input <- readFile "input.txt"
    let [(ingredients, "")] = readP_to_S (endBy parseIngredient (char '\n') <* eof) input
    
    -- Part 1
    print $ maximum $ map score $ bake 100 ingredients

    -- Part 2
    print $ maximum $ map score $ filter ((== 500) . sum . map (\(n, Ingredient{..}) -> n * calories)) $ bake 100 ingredients