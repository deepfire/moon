all: cli lift

.PHONY: lift cli cls clean
lift:
	cd lift
	bash -c "time cabal --enable-executable-dynamic -j new-build all"
	cabal new-exec   $@
cli vty:
	cd lift
	bash -c "time cabal --enable-executable-dynamic -j new-build exe:$@"
	cabal new-exec   $@ 2>stderr.log

cls:
	bash -c "echo -en '\ec'"
clean:
	cd lift
	cabal new-clean
