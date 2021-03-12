ARCH := $(shell uname -m)
OS := $(shell uname -s | tr '[:upper:]' '[:lower:]')
XMONAD_BINARY := xmonad-$(ARCH)-$(OS)

## compile xmonad.hs
xmonad: $(XMONAD_BINARY)

$(XMONAD_BINARY): build-deps
	cd ~/src-haskell && \
		stack exec -- ghc \
			-Wall \
			-i -i$(CURDIR) \
			-outputdir $(CURDIR)/.build/ \
			-threaded \
			-rtsopts -with-rtsopts="-V0" \
			-dynamic \
			-o $(CURDIR)/$@ \
			$(CURDIR)/xmonad.hs

.PHONY: ghcid
##
ghcid:
	ghcid -c '$(MAKE) $(TARGET)'

## make ghcid-xmonad; make ghcid-xmonad-contrib; …
ghcid-%: build-deps _phony
	cd ~/src-haskell && \
		ghcid --target $*:lib

.PHONY: ghci
##
ghci: build-deps
	cd ~/src-haskell && \
		stack exec -- ghci \
			-Wall \
			-i -i$(CURDIR) \
			$(CURDIR)/xmonad.hs

## make ghci-xmonad; make ghci-xmonad-contrib; …
ghci-%: build-deps _phony
	cd ~/src-haskell && \
		stack ghci $*:lib

.PHONY: build-deps
build-deps:
	$(MAKE) -C ~/src-haskell \
		BUILD_TARGETS="xmobar xmonad xmonad-contrib split" \
		build .bin/xmonadctl .bin/xmonadpropread .bin/xmonadpropwrite

.PHONY: build-doc
## build deps with docs
build-doc:
	$(MAKE) -C ~/src-haskell build-doc BUILD_TARGETS="xmobar xmonad xmonad-contrib split"

.PHONY: _phony

include ~/_help.mk
