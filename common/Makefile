all: module-graph

GRAPH := module-graph.pdf

module-graph graph: ${GRAPH}

${GRAPH}:
	graphmod --no-cabal src/Pipe.hs -isrc | dot -Tpdf > $@
	evince ${GRAPH} 2>/dev/null

.PHONY: ${GRAPH}
