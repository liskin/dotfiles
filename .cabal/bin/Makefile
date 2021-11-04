.PHONY: all
all:

INDEX := ../packages/hackage.haskell.org/01-index.timestamp

$(INDEX):
	cabal update

all: haskell-ci
haskell-ci: | $(INDEX)
	cd ~/src-haskell-cabal/haskell-ci && cabal install --overwrite-policy=always

all: cabal-store-gc
cabal-store-gc: | $(INDEX)
	cd ~/src-haskell-cabal/cabal-extras/cabal-store-gc && cabal install --overwrite-policy=always

include ~/_gitignore.mk
