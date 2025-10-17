all:
	wasm32-wasi-cabal build --allow-newer
	$(shell wasm32-wasi-ghc --print-libdir)/post-link.mjs \
		--input $(shell wasm32-wasi-cabal list-bin lab06-checkpoint --allow-newer) \
		--output app.wasmexe/ghc_wasm_jsffi.js
	cp -v $(shell wasm32-wasi-cabal list-bin lab06-checkpoint --allow-newer) app.wasmexe
