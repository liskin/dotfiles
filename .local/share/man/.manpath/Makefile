.PHONY: all
all:

all: rustup/share
rustup/share:
	mkdir -p $(@D)
	ln -fsn ../../../../../.rustup/toolchains/stable-$(shell uname -m)-unknown-linux-gnu/share $@

MISE_SHIMS_FAST=~/bin/.mise-shims-fast

all: neovim/share
neovim/share:
	mkdir -p $(@D)
	ln -fsn "$(shell env -C $(MISE_SHIMS_FAST) realpath --no-symlinks --canonicalize-missing --relative-to=$(CURDIR)/$(@D) $(shell readlink $(MISE_SHIMS_FAST)/nvim)/../../share)" $@

include ~/_gitignore.mk

.DELETE_ON_ERROR:
