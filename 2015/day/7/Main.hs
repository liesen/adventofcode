{-# LANGUAGE ViewPatterns, LambdaCase #-}
import Data.Bits
import Data.Char
import Data.Word
import qualified Data.Map as Map
import Data.Map (Map, (!))

data Expr
    = Const Word16
    | Wire String
    | And Expr Expr
    | Or Expr Expr
    | Lshift Expr Int
    | Rshift Expr Int
    | Not Expr
  deriving (Show)

signal (span isDigit -> (read -> i, ' ':s)) = (Const i, s)
signal (span isLower -> (x, ' ':s)) = (Wire x, s)

arrow ('-':'>':' ':x) = x

parse1 ('N':'O':'T':' ':(signal -> (sig, arrow -> z))) = (z, Not sig)
parse1 (signal -> (e, 'A':'N':'D':' ':(signal -> (f, arrow -> z)))) = (z, And e f)
parse1 (signal -> (e, 'O':'R':' ':(signal -> (f, arrow -> z)))) = (z, Or e f)
parse1 (signal -> (e, 'L':'S':'H':'I':'F':'T':' ':(signal -> (Const n, arrow -> z)))) = (z, Lshift e (fromIntegral n))
parse1 (signal -> (e, 'R':'S':'H':'I':'F':'T':' ':(signal -> (Const n, arrow -> z)))) = (z, Rshift e (fromIntegral n))
parse1 (signal -> (e, arrow -> z)) = (z, e)

parse = map parse1 . lines

eval expr circuit = go mempty expr
    where
        go env = \case
            Const i -> (i, env)
            Wire w -> case Map.lookup w env of
                        Nothing ->
                            case lookup w circuit of
                                Nothing -> error ("no signal provided to wire " ++ w)
                                Just expr -> 
                                    let (res, env') = go env expr
                                    in (res, Map.insert w res env')
                        Just res -> (res, env)
            Not e -> let (a, env') = go env e
                     in (complement a, env')
            And e f -> let (a, env') = go env e 
                           (b, env'') = go env' f
                       in (a .&. b, env'')
            Or e f -> let (a, env') = go env e 
                          (b, env'') = go env' f
                      in (a .|. b, env'')
            Lshift e n -> let (a, env') = go env e in (a `shiftL` n, env')
            Rshift e n -> let (a, env') = go env e in (a `shiftR` n, env')

main = do
    input <- readFile "input.txt"
    let circuit = parse input
        Just a = lookup "a" circuit

    -- Part 1
    let (ans1, _) = eval a circuit
    print ans1

    -- Part 2
    let (ans2, _) = eval a (("b", Const 16076):filter ((/= "b") . fst) circuit)
    print ans2
