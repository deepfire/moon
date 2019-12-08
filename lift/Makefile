all: cli lift

lift:
	bash -c "time cabal --enable-executable-dynamic -j new-build all"
	cabal new-exec   $@
cli vty:
	bash -c "time cabal --enable-executable-dynamic -j new-build exe:$@"
	cabal new-exec   $@ 2>stderr.log

cls:
	bash -c "echo -en '\ec'"
clean:
	cabal new-clean
