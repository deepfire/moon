all: cli lift

.PHONY: lift cli cls clean
lift:
	cd lift
	bash -c "time cabal -j build all"
	cabal exec   $@ daemon
cli vty xp:
	cd lift
	bash -c "time cabal -j build exe:$@"
	cabal exec   $@ 2>stderr.log || true
	cat stderr.log

ghci-parse:
	true ## cabal v2-repl cli -O0
	cabal v2-repl cli -O0 --repl-options=-ghci-script=lift/.ghci.parse
	true ## cabal v2-repl cli -O0 --ghc-option -ghci-script=lift/.ghci.parse
	true ## Why the fucking hell does that not load the specified file?

debug:
	cabal --ghc-options=-dshow-passes --ghc-options=-dppr-debug --ghc-options=-ddump-rn --ghc-options=-ddump-tc build 'exe:cli'

kill:
	pgrep -fal ghc
	pkill -9 ghc

cls:
	bash -c "echo -en '\ec'"
clean:
	cd lift
	cabal new-clean


bug git-bug:
	git-bug termui

ENTRY := lift/MainCLI.hs
GRAPH := module-graph.pdf


module-graph graph: ${GRAPH}

GRAPH_IGNORES =
GRAPH_IGNORES += --remove-qual Data
GRAPH_IGNORES += --remove-qual Debug
GRAPH_IGNORES += --remove-qual Generics
GRAPH_IGNORES += --remove-qual Reflex
GRAPH_IGNORES += --remove-module Basis
${GRAPH}:
	graphmod --no-cabal ${ENTRY} ${GRAPH_IGNORES} -ilift/src -icommon/src | dot -Tpdf > $@
	evince $@ 2>/dev/null

.PHONY: ${GRAPH}
