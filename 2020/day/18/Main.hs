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

expr :: ReadP Expr
expr = chainl1 (skipSpaces *> term) (skipSpaces *> op)
  where
    op = add <++ mul

term :: ReadP Expr
term = lit <++ parens expr

parens = between (char '(') (char ')')

eval :: Expr -> Int
eval (Lit n)   = n
eval (Add l r) = eval l + eval r
eval (Mul l r) = eval l * eval r

main :: IO ()
main = do
    input <- readFile "input.txt"
    let [(exprs, "")] = readP_to_S (expr `endBy` char '\n' <* eof) input

    -- Part 1
    print $ getSum $ foldMap (Sum . eval) exprs
