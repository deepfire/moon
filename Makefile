all: cli moon

cli moon:
	cabal new-build exe:$@
	cabal new-run       $@

clean:
	cabal new-clean
