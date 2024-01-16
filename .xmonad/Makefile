ARCH := $(shell uname -m)
OS := $(shell uname -s | tr '[:upper:]' '[:lower:]')
XMONAD_DEPS := xmobar xmonad xmonad-contrib split regex
BUILD_TARGETS = $(XMONAD_DEPS)
BUILD_DEPS_TARGETS := build .bin/xmonadctl .bin/xmonadpropread .bin/xmonadpropwrite
BUILD_DOCS_TARGETS := build-doc
SRC_HASKELL := ~/src-haskell

## compile xmonad.hs
all: $(patsubst %.hs,%-$(ARCH)-$(OS),$(wildcard *.hs))

%-$(ARCH)-$(OS): %.hs build-deps
	cd $(SRC_HASKELL) && \
		stack ghc -- \
			-Wall \
			-i -i$(CURDIR) \
			-outputdir $(CURDIR)/.build/ \
			-threaded \
			-rtsopts -with-rtsopts="-V0" \
			-dynamic \
			-o $(CURDIR)/$@ \
			-main-is Main$* \
			$(CURDIR)/$<

.PHONY: ghcid
ghcid:
	ghcid -c '$(MAKE) ghci'

ghcid-X11:
ghcid-X11-xft:
ghcid-xmonad-contrib:
ghcid-xmonad:
ghcid-xmobar:
ghcid-%: build-deps _phony
	cd $(SRC_HASKELL) && \
		ghcid --target $*:lib

ghcid-test-xmonad-contrib:
ghcid-test-%: build-deps _phony
	cd $(SRC_HASKELL) && \
		ghcid --target $*:test:tests --allow-eval

.PHONY: ghci
ghci: build-deps
	cd $(SRC_HASKELL) && \
		stack exec -- ghci \
			-Wall \
			-i -i$(CURDIR) \
			$(wildcard $(CURDIR)/*.hs)

ghci-X11:
ghci-X11-xft:
ghci-xmonad-contrib:
ghci-xmonad:
ghci-xmobar:
ghci-%: build-deps _phony
	cd $(SRC_HASKELL) && \
		stack ghci $*:lib

ghci-test-xmonad-contrib:
ghci-test-%: build-deps _phony
	cd $(SRC_HASKELL) && \
		stack ghci $*:test:tests

.PHONY: build-deps
build-deps:
	$(MAKE) -C $(SRC_HASKELL) BUILD_TARGETS="$(BUILD_TARGETS)" $(BUILD_DEPS_TARGETS)

.PHONY: build-doc
## build deps with docs
build-doc:
	$(MAKE) -C $(SRC_HASKELL) BUILD_TARGETS="$(BUILD_TARGETS)" $(BUILD_DOCS_TARGETS)

.PHONY: _phony

include ~/_help.mk
