configure-dev:
	cabal v2-configure --enable-tests

build:
	cabal v2-build

tests:
	cabal v2-test
