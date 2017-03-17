ROOT=$(shell stack path --local-install-root)
EXTERNS=$(shell find nodejs-externs/ -maxdepth 1 -name '*.js' | sed 's/^/--externs=/')

all: hs.min.js

hs.jsexe: src hs.cabal stack.yaml
	stack build

hs.js: hs.jsexe
	cat $(ROOT)/bin/hs.jsexe/{rts,lib,out}.js > hs.js
	sed 's/^h\$$main(/h\$$runSync(/' $(ROOT)/bin/hs.jsexe/runmain.js >> hs.js

hs.min.js: hs.js
	closure-compiler hs.js --compilation_level=ADVANCED_OPTIMIZATIONS $(EXTERNS) --externs=$(ROOT)/bin/hs.jsexe/all.js.externs > hs.min.js

.PHONY: clean
clean:
	stack clean hs
	rm -f hs{.min,}.js
