Cabal.jsexe/out.js: Cabal.hs
	# ghcjs rev. 561365ba1667053b5dc5846e2a8edb33eaa3f6dd
	ghcjs -DGHCJS_BROWSER Cabal.hs

HaskellCabal.js: Cabal.jsexe/out.js Exports.js
	echo "(function(global) {" >HaskellCabal.js
	cat Cabal.jsexe/{rts,lib,out}.js Exports.js >>HaskellCabal.js
	echo "})(exports);" >>HaskellCabal.js

HaskellCabal.min.js: Cabal.jsexe/out.js Exports.js
	echo "(function(global) {" >HaskellCabal.min.js
	../node_modules/.bin/ccjs Cabal.jsexe/{rts,lib,out}.js Exports.js \
	  --compilation_level=ADVANCED_OPTIMIZATIONS \
		--externs=node \
	  >>HaskellCabal.min.js
	echo "})(exports);" >>HaskellCabal.min.js

.PHONY: clean
clean:
	rm -rf Cabal.jsexe
	rm -f HaskellCabal.{js,min.js}
	rm -f *.js_{hi,o}
