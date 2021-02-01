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
