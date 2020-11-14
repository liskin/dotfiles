ARCH := $(shell uname -m)
OS := $(shell uname -s | tr '[:upper:]' '[:lower:]')
XMONAD_BINARY := xmonad-$(ARCH)-$(OS)

all: $(XMONAD_BINARY) dep/xmobar/examples/xmonadpropwrite

$(XMONAD_BINARY): stack
	stack exec -- ghc --make xmonad.hs -fforce-recomp -rtsopts -threaded -with-rtsopts="-V0" -o $@.tmp
	mv -f $@.tmp $@

dep/xmobar/examples/xmonadpropwrite: stack
	stack exec -- ghc --make dep/xmobar/examples/xmonadpropwrite.hs

ghcid: stack
	ghcid -c 'stack exec -- ghci xmonad.hs'

stack:
	stack build --copy-bins --local-bin-path "$(shell pwd)/bin" \
		xmobar \
		xmonad \
		xmonad-contrib \
		split
