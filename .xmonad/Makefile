ARCH := $(shell uname -m)
OS := $(shell uname -s | tr '[:upper:]' '[:lower:]')
XMONAD_BINARY := xmonad-$(ARCH)-$(OS)
XMONAD_DEPS := xmobar xmonad xmonad-contrib split regex-compat-tdfa
BUILD_TARGETS = $(XMONAD_DEPS)
BUILD_DEPS_TARGETS := build .bin/xmonadctl .bin/xmonadpropread .bin/xmonadpropwrite
BUILD_DOCS_TARGETS := build-doc
SRC_HASKELL := ~/src-haskell

## compile xmonad.hs
all: $(XMONAD_BINARY)

$(XMONAD_BINARY): build-deps
	cd $(SRC_HASKELL) && \
		stack ghc -- \
			-Wall \
			-i -i$(CURDIR) \
			-outputdir $(CURDIR)/.build/ \
			-threaded \
			-rtsopts -with-rtsopts="-V0" \
			-dynamic \
			-o $(CURDIR)/$@ \
			$(CURDIR)/xmonad.hs

.PHONY: ghcid
ghcid:
	ghcid -c '$(MAKE) ghci'

ghcid-X11:
ghcid-xmonad-contrib:
ghcid-xmonad:
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
			$(CURDIR)/xmonad.hs

ghci-X11:
ghci-xmonad-contrib:
ghci-xmonad:
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
