.PHONY: run
	
Main: Main.hs
	ghc -package containers --make Main.hs

# runghc --ghc-arg="-package containers" Main.hs
run: Main
	time ./Main
