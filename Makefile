SHELL := bash
.ONESHELL:
.SHELLFLAGS := -eu -o pipefail -c

GHC  ?= 9.6
MAIN ?= Day01.Main
EXE  := $(shell basename $(MAIN) .Main)

.PHONY: exe install-bin install-lib hiedb watch
exe:
	ghc-$(GHC) -isrc $(MAIN) -main-is $(MAIN) -o /tmp/$(EXE)

env:
	rm .ghc.environment.*$(GHC)* || true
	cabal install -w ghc-$(GHC) --allow-newer --package-env . --lib \
		base relude bluefin conduit foldl io-classes \
		aeson optics aeson-optics optparse-generic pretty-show \
		template-haskell ihp-hsx lucid2 rapid twain wai wai-extra warp \
		auto-split breakpoint tasty tasty-hunit tasty-wai io-sim

hie:
	hiedb-$(GHC) -D .hiedb index .hiefiles --src-base-dir .

watch:
	ghciwatch --clear --no-interrupt-reloads \
		--command ghci-$(GHC) \
		--after-reload-shell 'make hie' \
		--error-file ghcid.txt \
		--enable-eval --watch .

