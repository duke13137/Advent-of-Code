SHELL := bash
.ONESHELL:
.SHELLFLAGS := -eu -o pipefail -c

GHC  ?= 9.10
MAIN ?= Day01.Main
EXE  := $(shell basename $(MAIN) .Main)

.PHONY: exe install-bin install-lib hiedb watch
exe:
	ghc-$(GHC) -isrc $(MAIN) -main-is $(MAIN) -o /tmp/$(EXE)

env:
	rm .ghc.environment.*$(GHC)* || true
	cabal install -w ghc-$(GHC) --allow-newer --package-env . --lib \
		base relude bytestring text containers unordered-containers \
		bluefin conduit foldl io-classes io-sim stm stm-containers \
		aeson optics aeson-optics optparse-generic pretty-show strict-wrapper \
		co-log lucid2 twain wai wai-extra warp template-haskell \
		auto-split breakpoint rapid tasty tasty-hunit tasty-wai

hie:
	hiedb-$(GHC) -D .hiedb index .hiefiles --src-base-dir .

watch:
	ghciwatch --clear --no-interrupt-reloads \
		--command ghci-$(GHC) \
		--after-reload-shell 'make hie' \
		--error-file ghcid.txt \
		--enable-eval --watch .

