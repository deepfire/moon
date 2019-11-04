all: cli lift

lift:
	bash -c "time cabal -j new-build all"
	cabal new-exec   $@
cli vty:
	bash -c "time cabal -j new-build exe:$@"
	cabal new-exec   $@

cls:
	bash -c "echo -en '\ec'"
clean:
	cabal new-clean
