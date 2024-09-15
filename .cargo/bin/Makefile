.PHONY: all
all:

include ~/_git.mk
include ~/_gitignore.mk
include ~/_help.mk

LIVEBUDSCLI_SRC := ~/src-rust/LiveBudsCli
LIVEBUDSCLI_VERSION := $(shell $(call GIT_DESCRIBE,$(LIVEBUDSCLI_SRC)))

# FIXME: drop once https://github.com/jdx/mise/issues/2583 fixed
all: earbuds
earbuds: $(MAKEFILE_LIST) | .build/earbuds ; ln -fsT $|/bin/$@ $@; touch $@
.build/earbuds: $(MAKEFILE_LIST) | .build/earbuds-$(LIVEBUDSCLI_VERSION) ; ln -fsrT $| $@; touch $@
.build/earbuds-$(LIVEBUDSCLI_VERSION):
	cargo install --root .build/earbuds-$(LIVEBUDSCLI_VERSION) --path ~/src-rust/LiveBudsCli

.PHONY: gc
## Remove old versions
gc:
	liskin-gc-unlinked $(wildcard .build/*)
