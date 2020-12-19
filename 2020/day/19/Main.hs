import Control.Applicative
import Data.Char
import Data.List
import Data.Monoid hiding (Alt)
import Text.ParserCombinators.ReadP
import Prelude hiding (seq)

data Re
    = Ref String
    | Lit String
    | Seq [Re]
    | Alt [Re]
  deriving (Show)

ref, lit, seq, alt, re :: ReadP Re
ref = Ref <$> munch1 isDigit
lit = Lit <$> between (char '"') (char '"') (munch1 isAlpha)
seq = Seq <$> ref `sepBy` char ' '
alt = Alt <$> seq `sepBy` string " | "
re = alt +++ lit

rule :: ReadP (String, Re)
rule = (,) <$> munch1 isDigit <*> (string ": " *> re)

parse :: ReadP ([(String, Re)], [String])
parse = (,) <$> rule `endBy` char '\n' <*> (char '\n' *> munch1 isAlpha `endBy` char '\n')

-- Take the expression and turn it into a ReadP
compile :: [(String, Re)] -> Re -> ReadP String
compile rules (Lit x)  = string x
compile rules (Alt xs) = choice (map (compile rules) xs)
compile rules (Seq xs) = concat <$> mapM (compile rules) xs
compile rules (Ref r)  =
    case lookup r rules of
        Nothing -> pfail
        Just re -> compile rules re

matches :: ReadP String -> String -> Bool
matches p line = readP_to_S (p <* eof) line == [(line, "")]

main = do
    input <- readFile "input.txt"
    let [((rules, lines), "")] = readP_to_S (parse <* eof) input
    let Just rule0 = lookup "0" rules

    -- Part 1
    let p = compile rules rule0
    print $ length $ filter (matches p) lines

    -- Part 2
    let rules2 = filter ((`notElem` ["8", "11"]) . fst) rules 
               ++ [("8", Alt [Seq [Ref "42"],Seq [Ref "42",Ref "8"]]),
                   ("11", Alt [Seq [Ref "42",Ref "31"],Seq [Ref "42",Ref "11",Ref "31"]])]
        p = compile rules2 rule0
    print $ length $ filter (matches p) lines
