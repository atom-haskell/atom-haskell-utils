all: HaskellCabal.js HaskellCabal.min.js

Cabal.jsexe/out.js: Cabal.hs
	# ghcjs rev. 561365ba1667053b5dc5846e2a8edb33eaa3f6dd
	ghcjs -DGHCJS_BROWSER Cabal.hs

Exports.js: Cabal.jsexe/out.js
	sed 's/^h\$$main(/h\$$runSync(/' Cabal.jsexe/runmain.js > Exports.js

HaskellCabal.js: Cabal.jsexe/out.js Exports.js
	cat Cabal.jsexe/{rts,lib,out}.js Exports.js >HaskellCabal.js

HaskellCabal.min.js: Cabal.jsexe/out.js Exports.js
	../node_modules/.bin/ccjs Cabal.jsexe/{rts,lib,out}.js Exports.js \
	  --compilation_level=ADVANCED_OPTIMIZATIONS \
		--externs=node \
	  >HaskellCabal.min.js

.PHONY: clean
clean:
	rm -rf Cabal.jsexe
	rm -f HaskellCabal.{js,min.js}
	rm -f *.js_{hi,o}
