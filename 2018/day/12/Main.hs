{-# LANGUAGE BangPatterns #-}
import Data.Bits
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Word
import Text.ParserCombinators.ReadP
import System.IO


data Pots = Pots
    { state :: Set Int  -- Pot indices
    , minIx :: !Int  -- Smallest index (could use Set.findMin)
    , maxIx :: !Int  -- Largest index (Set.findMax)
    } deriving (Show)

instance Monoid Pots where
    mempty = Pots mempty 0 0 
    mappend (Pots s0 m0 n0) (Pots s1 m1 n1) =
        Pots (s0 `mappend` s1) (min m0 m1) (max n0 n1)

newtype Rules = Rules (Set Word8)

pot '#' = True
pot '.' = False

checksumBy f = foldl setBit 0 . map fst . filter (f . snd) . zip [0..]

parse = do
    s <- string "initial state: " *> manyTill get (char '\n')
    _ <- char '\n'
    r <- sepBy parseRule (char '\n')
    _ <- char '\n'
    eof
    let initialState = Set.fromAscList $ map fst $ filter (pot . snd) $ zip [0..] s
        pots = Pots initialState (minimum initialState) (maximum initialState)
        rules = Rules $ Set.fromList $ concatMap ruleBits r
    return (pots, rules)
  where
    parseRule = do
        rule <- count 5 get
        _ <- string " => "
        y <- get
        return (rule, y)

    ruleBits (rule, y)
        | pot y     = return $ checksumBy pot rule
        | otherwise = fail "irrelevant rule"

step :: Rules -> Pots -> Pots
step (Rules rules) (Pots s m n) =
    foldr (\i pots@(Pots s' m' n') ->
               if test s rules i
                   then insert i pots
                   else pots)
        mempty
        [m - 2..n + 2]
  where
    test s rules i = let b = checksumBy (flip Set.member s) [i - 2..i + 2]
                     in Set.member b rules
    insert i (Pots s m n) = Pots (Set.insert i s) (min m i) (max n i)

score (Pots s _ _) = sum $ Set.toList s

main = do
    input <- readFile "input.txt"
    let [((s0, rules), [])] = readP_to_S parse input

    -- Part 1
    print $ score $ iterate (step rules) s0 !! 1000

    -- Part 2
    -- This will reveal the pattern: score = generation * 40 + 1684
    {-
    hSetBuffering stdout NoBuffering
    mapM_ (\(i, p) -> if i `mod` 1000 == 0
                          then do putStr $ show i
                                  putChar ','
                                  putStrLn $ show (score p)
                          else return ())
          (zip [0..] (iterate (step rules) s0))
    -}
    print $ 50000000000 * 40 + 1682
