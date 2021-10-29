.PHONY: all
all:

all: haskell-ci
haskell-ci:
	cabal update
	cd ~/src-haskell-cabal/haskell-ci && cabal install --overwrite-policy=always

include ~/_gitignore.mk
