SHELL := bash
.ONESHELL:
.SHELLFLAGS := -eu -o pipefail -c

GHC  ?= 9.10
MAIN ?= Main
EXE  := $(shell basename $(MAIN) .Main)

.PHONY: exe env hie pg watch

exe:
	ghc-$(GHC) -isrc -outputdir .ghcfiles -main-is $(MAIN) -o .ghcfiles/$(EXE) $(MAIN)

env:
	rm .ghc.environment.*$(GHC)* || true
	cabal install -w ghc-$(GHC) --allow-newer --package-env . --lib \
		base relude bytestring text containers unordered-containers \
		aeson optics aeson-optics optparse-generic pretty-show strict-wrapper \
		bluefin conduit foldl io-classes io-sim stm stm-containers \
		hasql hasql-th hasql-implicits hasql-pool hasql-transaction \
		co-log lucid2 twain wai wai-extra warp template-haskell \
		auto-split breakpoint rapid tasty tasty-hunit tasty-wai

hie:
	hiedb-$(GHC) -D .hiedb index .ghcfiles

pg:
	docker run --name postgres \
		-e POSTGRES_PASSWORD=$(shell cat /var/run/pgsecret) \
		-p 5432:5432 \
		-d postgres

watch:
	ghciwatch --clear --no-interrupt-reloads \
		--command ghci-$(GHC) \
		--after-reload-shell 'make hie' \
		--error-file ghcid.txt \
		--enable-eval --watch .
