SHELL := bash

define BINS
	gh
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
	global=$$(sed -e 's|\(mise/installs/[^/]*/\)[0-9.]*|\1global|' <<<"$$which") && \
	relative=$$(realpath --no-symlinks --relative-to=. "$$global") && \
	ln -fns "$$relative" $@

include ~/_gitignore.mk

.DELETE_ON_ERROR:
