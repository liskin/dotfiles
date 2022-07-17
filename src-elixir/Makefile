.PHONY: all
all:

include ~/_git.mk
include ~/_help.mk

ELIXIR_SRC := elixir
ELIXIR_VERSION := $(shell $(call GIT_DESCRIBE,$(ELIXIR_SRC)))

all: .build/elixir
.build/elixir: $(MAKEFILE_LIST) | .build/elixir-$(ELIXIR_VERSION) ; ln -fsrT $| $@; touch $@
.build/elixir-$(ELIXIR_VERSION):
	mkdir -p $@
	$(MAKE) -C $(ELIXIR_SRC) install PREFIX=$(CURDIR)/$@

ELIXIR_LS_SRC := elixir-ls
ELIXIR_LS_VERSION := $(shell $(call GIT_DESCRIBE,$(ELIXIR_LS_SRC)))

all: .build/elixir-ls
.build/elixir-ls: $(MAKEFILE_LIST) | .build/elixir-ls-$(ELIXIR_LS_VERSION) ; ln -fsrT $| $@; touch $@
.build/elixir-ls-$(ELIXIR_LS_VERSION):
	cd $(ELIXIR_LS_SRC) && \
		mix deps.get && \
		mix elixir_ls.release -o $(CURDIR)/$@

.PHONY: gc
## Remove old versions
gc:
	liskin-gc-unlinked $(wildcard .build/*)
