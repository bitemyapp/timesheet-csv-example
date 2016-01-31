run: deps
	stack exec -- runhaskell Main.hs

deps:
	stack build trifecta cassava
