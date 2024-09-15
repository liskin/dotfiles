.PHONY: all
all: shims/cargo-binstall
	mise install

# FIXME: workaround for https://github.com/jdx/mise/issues/2260
shims/cargo-binstall:
	mise install ubi:cargo-bins/cargo-binstall

.PHONY: gc
gc:
	mise prune
