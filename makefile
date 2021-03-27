configure-dev:
	cabal v2-configure --enable-tests

build:
	cabal v2-build

tests:
	cabal v2-test

release:
	cabal v2-haddock --haddock-for-hackage --enable-doc
