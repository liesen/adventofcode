import Data.Char
import Data.Monoid
import Text.ParserCombinators.ReadP

data Expr
  = Lit Int
  | Add Expr Expr
  | Mul Expr Expr
  deriving (Show)

lit :: ReadP Expr
lit = Lit . read <$> munch1 isDigit

add, mul :: ReadP (Expr -> Expr -> Expr)
add = Add <$ char '+'
mul = Mul <$ char '*'

expr1 :: ReadP Expr
expr1 = chainl1 (skipSpaces *> term1) (skipSpaces *> op)
  where
    op = add +++ mul

term1 :: ReadP Expr
term1 = lit +++ parens expr1

parens = between (char '(') (char ')')

eval :: Expr -> Int
eval (Lit n)   = n
eval (Add l r) = eval l + eval r
eval (Mul l r) = eval l * eval r

evalLine :: ReadP Expr -> String -> Int
evalLine p = eval . fst . head . readP_to_S (p <* eof)

-- https://en.wikibooks.org/wiki/Haskell/ParseExps#Structure_Emerges
expr2 = foldr (\op p ->
                    let this = p +++ do a <- p +++ parens expr2
                                        f <- skipSpaces *> op
                                        f a <$> (skipSpaces *> this)
                    in this)
              (lit +++ parens expr2)
              [mul, add]

main :: IO ()
main = do
    input <- readFile "input.txt"

    -- Part 1
    print $ getSum $ foldMap (Sum . evalLine expr1) $ lines input

    -- Part 2
    print $ getSum $ foldMap (Sum . evalLine expr2) $ lines input
