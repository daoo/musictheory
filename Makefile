all:
	@cabal build --ghc-options="-Wall"

test:
	@cabal test --ghc-options="-Wall"

lint:
	hlint src tests
