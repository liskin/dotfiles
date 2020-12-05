ARCH := $(shell uname -m)
OS := $(shell uname -s | tr '[:upper:]' '[:lower:]')
XMONAD_BINARY := xmonad-$(ARCH)-$(OS)

$(XMONAD_BINARY): build-deps
	cd ~/src-haskell && \
		stack exec -- ghc --make $(CURDIR)/xmonad.hs -fforce-recomp -rtsopts -threaded -with-rtsopts="-V0" -o $(CURDIR)/$@.tmp
	mv -f $@.tmp $@

.PHONY: ghcid
ghcid: build-deps
	cd ~/src-haskell && \
		ghcid -c 'stack exec -- ghci $(CURDIR)/xmonad.hs'

.PHONY: build-deps
build-deps:
	$(MAKE) -C ~/src-haskell build BUILD_TARGETS="xmobar xmonad xmonad-contrib split"
	cd ~/src-haskell && \
		stack exec -- ghc --make xmobar/examples/xmonadpropwrite.hs && \
		cp xmobar/examples/xmonadpropwrite .bin/
