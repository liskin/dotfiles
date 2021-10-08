BUILD_TARGETS ?=
STACK_BUILDFLAGS ?=

.PHONY: build
build:
	stack build \
		--copy-bins --local-bin-path "$(CURDIR)/.bin" \
		$(STACK_BUILDFLAGS) \
		$(BUILD_TARGETS)

.PHONY: build-doc
build-doc: STACK_BUILDFLAGS=--haddock --no-haddock-deps --no-haddock-hyperlink-source --haddock-arguments --optghc=-dynamic
build-doc: build

.bin/xmonadctl: xmonad-contrib/scripts/xmonadctl.hs | build
	stack ghc -- \
		-dynamic \
		-outputdir ~/.xmonad/.build/xmonadctl/ \
		-o .bin/xmonadctl $<

.bin/xmonadpropread: xmonad-contrib/scripts/xmonadpropread.hs | build
	stack ghc -- \
		-dynamic \
		-outputdir ~/.xmonad/.build/xmonadpropread/ \
		-o .bin/xmonadpropread $<

.bin/xmonadpropwrite: xmobar/examples/xmonadpropwrite.hs | build
	stack ghc -- \
		-dynamic \
		-outputdir ~/.xmonad/.build/xmonadpropwrite/ \
		-o .bin/xmonadpropwrite $<

.DELETE_ON_ERROR:
