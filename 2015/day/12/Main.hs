import Data.Aeson
import Data.Monoid
import Data.Scientific
import qualified Data.Text as Text
import qualified Data.HashMap.Strict as HashMap

-- Sums all the numbers in a JSON object
numberSum :: Value -> Scientific
numberSum = getSum . go
  where
    go (Object o) = foldMap go o
    go (Array a)  = foldMap go a
    go (Number n) = Sum n
    go _          = Sum 0

integerSum = either (const Nothing) (Just . toInteger) . floatingOrInteger . numberSum

-- Replace all maps which has a value "red" with null
filterNotRed (Object o)
    | elem red (HashMap.elems o) = Null
    | otherwise = Object (fmap filterNotRed o)
  where
    red = String (Text.pack "red")
filterNotRed (Array a) = Array (fmap filterNotRed a)
filterNotRed x = x

main = do
    Just input <- decodeFileStrict "input.txt"

    -- Part 1
    let Just ans1 = integerSum input
    print ans1

    -- Part 2
    let Just ans2 = integerSum (filterNotRed input)
    print ans2

    