BROWSERIFY := ./node_modules/browserify/bin/cmd.js
ROLLUP     := ./node_modules/rollup/bin/rollup

JSEXE      := ../../dist-newstyle/build/x86_64-linux/ghcjs-8.6.0.1/node-editor-0.1.0.0/x/node-editor/build/node-editor/node-editor.jsexe

ZIPFSROOT   := js/zipfs
ZIPFS       := $(ZIPFSROOT)/zipfs.zip
ZIPFSBASE64 := $(ZIPFSROOT)/zipfs.base64
ZIPJS       := js/zipfs.js

all: lib-suffix main

zipfs: $(ZIPFS)
$(ZIPFS):
	rm -f $(ZIPFS)
	cd $(ZIPFSROOT) && zip zipfs.zip -r js

base64: $(ZIPFSBASE64)
$(ZIPFSBASE64): $(ZIPFS)
	base64 -w0 $< > $@

zipjs: $(ZIPJS)
$(ZIPJS): $(ZIPFSBASE64)
	echo -n 'var zipfs = "' > $@
	cat $<                 >> $@
	echo '";'              >> $@

browserified: js/browserified.js
js/browserified.js: js/pre-browserified.js run-browserify.js package.json
	# $(BROWSERIFY) $< --outfile $@ -r react -r react-dom -r ./js/atom-callback.js -r ./js/luna-visualizers.js
	node run-browserify.js $< > $@

dynamic: js/lib-suffix-dynamic.js
js/lib-suffix-dynamic.js: $(ZIPJS) js/browserified.js js/lib-suffix-dynamic.suffix.js
	cat $^ > $@

lib-suffix: $(JSEXE)/lib-suffix.js
$(JSEXE)/lib-suffix.js: js/lib-suffix-static.js js/lib-suffix-dynamic.js
	cat $^ > $@

main: $(JSEXE)/lib.js
$(JSEXE)/lib.js: node-editor.cabal
	cabal new-build exe:node-editor
