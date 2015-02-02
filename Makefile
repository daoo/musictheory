all:
	@cabal build --ghc-options="-Wall"

test:
	@cabal configure --enable-tests
	@cabal build --ghc-options="-Wall" musictheory-doctest
	@./dist/build/musictheory-doctest/musictheory-doctest

lint:
	hlint src tests
