SHELL := bash

define BINS
	gh
	just
	nvim
	ruff
	usage
	uv
	uvx
endef

.PHONY: all
all: $(strip $(BINS))

$(strip $(BINS)):
	which=$$(mise which $@) && \
	latest=$$(sed -e 's|\(mise/installs/[^/]*/\)[0-9.]*|\1latest|' <<<"$$which") && \
	relative=$$(realpath --no-symlinks --relative-to=. "$$latest") && \
	ln -fns "$$relative" $@

include ~/_gitignore.mk

.DELETE_ON_ERROR:
