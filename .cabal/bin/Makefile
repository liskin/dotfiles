.PHONY: all
all:

INDEX := ../packages/hackage.haskell.org/01-index.timestamp

$(INDEX):
	cabal update

all: haskell-ci
haskell-ci: | $(INDEX)
	cd ~/src-haskell-cabal/haskell-ci && cabal install --overwrite-policy=always

include ~/_gitignore.mk
