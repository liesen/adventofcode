import Data.Char
import Text.ParserCombinators.ReadP
import Data.IntMap (IntMap)
import qualified Data.IntMap.Strict as IntMap

number :: ReadP Int
number = read <$> munch1 isDigit

parse = number `sepBy` char ','

{- Too slow...
type State = (Int, (Int, IntMap Int))

step :: State -> State
step (prev, (i, seen)) =
    (i - IntMap.findWithDefault i prev seen, (i + 1, IntMap.insert prev i seen))
-}

data State' = State' !Int !Int !(IntMap Int)

step' :: State' -> State'
step' (State' prev i seen) =
    State' (i - IntMap.findWithDefault i prev seen)
           (i + 1)
           (IntMap.insert prev i seen)

main = do
    let input = "16,1,0,18,12,14,19"
        [(numbers, "")] = readP_to_S (parse <* eof) input
        init = IntMap.fromList $ zip numbers [0..]
        spokenNumbers' = numbers ++ map (\(State' n _ _) -> n) (iterate step' (State' 0 (length numbers) init))

    -- Part 1
    print (spokenNumbers' !! (2020 - 1))

    -- Part 2
    print (spokenNumbers' !! (30000000 - 1))