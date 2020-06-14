all: cli lift

.PHONY: lift cli cls clean
lift:
	cd lift
	bash -c "time cabal -j build all"
	cabal exec   $@
cli vty:
	cd lift
	bash -c "time cabal -j build exe:$@"
	cabal exec   $@ 2>stderr.log || true
	cat stderr.log

cls:
	bash -c "echo -en '\ec'"
clean:
	cd lift
	cabal new-clean
