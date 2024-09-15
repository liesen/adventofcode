.PHONY = run expr

run: Main.hs
	runghc -package --ghc-arg=mtl -package --ghc-arg=array Main.hs

expr: Expr.hs
	runghc -package --ghc-arg=mtl -package --ghc-arg=array -package --ghc-arg=split Expr.hs
