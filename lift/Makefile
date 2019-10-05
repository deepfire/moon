all: cli lift

cli lift:
	bash -c "time cabal -j new-build all"
	cabal new-exec   $@

cls:
	bash -c "echo -en '\ec'"
clean:
	cabal new-clean
