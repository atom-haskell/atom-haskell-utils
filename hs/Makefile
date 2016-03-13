ROOT=$(shell stack path --local-install-root)

all: hs.min.js

hs.js: Cabal.hs hs.cabal stack.yaml
	stack build
	cat $(ROOT)/bin/hs.jsexe/{rts,lib,out}.js > hs.js
	sed 's/^h\$$main(/h\$$runSync(/' $(ROOT)/bin/hs.jsexe/runmain.js >> hs.js

hs.min.js: hs.js
	../node_modules/.bin/ccjs hs.js \
		--compilation_level=SIMPLE_OPTIMIZATIONS \
		--externs=node \
		> hs.min.js

.PHONY: clean
clean:
	stack clean hs
	rm -f hs{.min,}.js
