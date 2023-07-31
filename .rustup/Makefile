.PHONY: all
all:

include ~/_help.mk

ifeq "$(wildcard toolchains/stable-*)" ""
all: install
endif

.PHONY: install
## Install/update rust toolchain
install:
	rustup toolchain install --no-self-update stable
