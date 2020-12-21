import Data.Char
import Data.List
import Text.ParserCombinators.ReadP

newtype Ingredient = Ingredient String deriving (Eq, Ord, Show)
newtype Allergen = Allergen String deriving (Eq, Ord, Show)

ingredient = Ingredient <$> munch1 isAlpha

allergen = Allergen <$> munch1 isAlpha

parseLine = (,) <$> (ingredient `sepBy` char ' ')
                <*> between (string " (contains ") (char ')') (allergen `sepBy` string ", ")

parseLines = parseLine `sepBy` char '\n'

translate :: [([Ingredient], [Allergen])] -> [(Allergen, Ingredient)]
translate foods = loop mempty $ sortOn (length . snd) candidates
  where
    ingredients = foldr1 union (map fst foods)
    allergens = foldr1 union (map snd foods)

    -- For each allergen, trim down the list of possible
    -- ingredient it can map to
    candidates = [ (a, foldr intersect ingredients [is | (is, as) <- foods, a `elem` as])
                 | a <- allergens
                 ]

    -- Extract translations that can be uniquely determined
    loop seen [] = []
    loop seen ((a, is):xs) =
        case filter (`notElem` seen) is of
            [x] -> (a, x) : loop (insert x seen) xs
            _   -> loop seen (xs ++ [(a, is)]) -- Try again later

main = do
    input <- readFile "input.txt"
    let [(foods, "")] = readP_to_S (parseLines <* char '\n' <* eof) input

    -- Part 1
    let translation = translate foods
        ingredients = foldr1 union (map fst foods)
        notAllergens = ingredients \\ map snd translation
    print $ length $ concatMap (\(is, as) -> filter (`elem` notAllergens) is) foods

    -- Part 2
    putStrLn $ intercalate "," $ map (\(Allergen a, Ingredient i) -> i) $ sortOn fst translation