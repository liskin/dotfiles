ARCH := $(shell uname -m)
OS := $(shell uname -s | tr '[:upper:]' '[:lower:]')
XMONAD_BINARY := xmonad-$(ARCH)-$(OS)

$(XMONAD_BINARY): build-deps
	cd ~/src-haskell && \
		stack exec -- ghc \
			-Wall \
			-i -i$(CURDIR) \
			-outputdir $(CURDIR)/.build/ \
			-threaded \
			-rtsopts -with-rtsopts="-V0" \
			-o $(CURDIR)/$@ \
			$(CURDIR)/xmonad.hs

.PHONY: ghcid
ghcid: TARGET=ghci
ghcid:
	ghcid -c '$(MAKE) $(TARGET)'

.PHONY: ghci
ghci: build-deps
	cd ~/src-haskell && \
		stack exec -- ghci \
			-Wall \
			-i -i$(CURDIR) \
			$(CURDIR)/xmonad.hs

ghci-%: build-deps _phony
	cd ~/src-haskell && \
		stack ghci $*:lib

.PHONY: build-deps
build-deps:
	$(MAKE) -C ~/src-haskell \
		BUILD_TARGETS="xmobar xmonad xmonad-contrib split" \
		build .bin/xmonadctl .bin/xmonadpropread .bin/xmonadpropwrite

.PHONY: build-doc
build-doc:
	$(MAKE) -C ~/src-haskell build-doc BUILD_TARGETS="xmobar xmonad xmonad-contrib split"

.PHONY: _phony
