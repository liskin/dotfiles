BUILD_TARGETS ?=
STACK_BUILDFLAGS ?=

.PHONY: build
build:
	stack build \
		--copy-bins --local-bin-path "$(CURDIR)/.bin" \
		$(STACK_BUILDFLAGS) \
		$(BUILD_TARGETS)

.PHONY: build-doc
build-doc: STACK_BUILDFLAGS=--haddock --no-haddock-deps --no-haddock-hyperlink-source
build-doc: build

.bin/xmonadpropread: | build
	stack exec -- ghc \
		-outputdir ~/.xmonad/.build/xmonadpropread/ \
		-o .bin/xmonadpropread \
		xmonad-contrib/scripts/xmonadpropread.hs

.bin/xmonadpropwrite: | build
	stack exec -- ghc \
		-outputdir ~/.xmonad/.build/xmonadpropwrite/ \
		-o .bin/xmonadpropwrite \
		xmobar/examples/xmonadpropwrite.hs
