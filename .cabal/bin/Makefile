.PHONY: all
all:

all: haskell-ci
haskell-ci:
	cabal update
	cd ~/src-haskell-cabal/haskell-ci && cabal install --overwrite-policy=always

all: cabal-store-gc
cabal-store-gc:
	cabal update
	cd ~/src-haskell-cabal/cabal-extras/cabal-store-gc && cabal install --overwrite-policy=always

include ~/_gitignore.mk
