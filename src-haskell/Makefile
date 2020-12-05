BUILD_TARGETS ?=

.PHONY: build
build:
	stack build --copy-bins --local-bin-path "$(CURDIR)/.bin" $(BUILD_TARGETS)
