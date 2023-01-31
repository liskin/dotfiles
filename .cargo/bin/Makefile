.PHONY: all
all:

include ~/_git.mk
include ~/_github.mk
include ~/_gitignore.mk
include ~/_help.mk

CARGO_CACHE_VERSION := 0.8.3
CARGO_EDIT_VERSION := v0.11.8
CARGO_WATCH_VERSION := v8.1.2
DUST_VERSION := v0.8.3
FCLONES_VERSION := v0.27.3

CARGO_CACHE_GITHUB_LASTTAG := matthiaskrgr/cargo-cache
CARGO_EDIT_GITHUB := killercup/cargo-edit
CARGO_WATCH_GITHUB := watchexec/cargo-watch
DUST_GITHUB := bootandy/dust
FCLONES_GITHUB := pkolaczk/fclones

.PHONY: update
## Check for updates
update: update-github

CARGO_BINSTALL := cargo binstall --no-confirm --disable-strategies compile --install-path .binstall

.binstall:
	mkdir -p $@

all: dust
dust: | .binstall/dust ; ln -fsT $| $@
.binstall/dust: .binstall/dust-$(DUST_VERSION)
.binstall/dust-$(DUST_VERSION): | .binstall
	$(CARGO_BINSTALL) --version =$(DUST_VERSION:v%=%) du-dust

all: cargo-watch
cargo-watch: | .binstall/cargo-watch ; ln -fsT $| $@
.binstall/cargo-watch: .binstall/cargo-watch-$(CARGO_WATCH_VERSION)
.binstall/cargo-watch-$(CARGO_WATCH_VERSION): | .binstall
	$(CARGO_BINSTALL) --version =$(CARGO_WATCH_VERSION:v%=%) cargo-watch

all: cargo-cache
cargo-cache: | .binstall/cargo-cache ; ln -fsT $| $@
.binstall/cargo-cache: .binstall/cargo-cache-v$(CARGO_CACHE_VERSION)
.binstall/cargo-cache-v$(CARGO_CACHE_VERSION): | .binstall
	$(CARGO_BINSTALL) --version =$(CARGO_CACHE_VERSION) cargo-cache

all: cargo-add cargo-rm cargo-set-version cargo-upgrade
cargo-add cargo-rm cargo-set-version cargo-upgrade: | .binstall/cargo-upgrade ; ln -fsT $(dir $|)$@ $@
.binstall/cargo-upgrade: .binstall/cargo-upgrade-$(CARGO_EDIT_VERSION)
.binstall/cargo-upgrade-$(CARGO_EDIT_VERSION): | .binstall
	$(CARGO_BINSTALL) --version =$(CARGO_EDIT_VERSION:v%=%) cargo-edit

all: fclones
fclones: $(MAKEFILE_LIST) | .build/fclones ; ln -fsT $|/bin/$@ $@; touch $@
.build/fclones: $(MAKEFILE_LIST) | .build/fclones-$(FCLONES_VERSION) ; ln -fsrT $| $@; touch $@
.build/fclones-$(FCLONES_VERSION):
	cargo install --root .build/fclones-$(FCLONES_VERSION) --version =$(FCLONES_VERSION:v%=%) --locked fclones

FRECE_SRC := ~/src-rust/frece
FRECE_VERSION := $(shell $(call GIT_DESCRIBE,$(FRECE_SRC)))

all: frece
frece: $(MAKEFILE_LIST) | .build/frece ; ln -fsT $|/bin/$@ $@; touch $@
.build/frece: $(MAKEFILE_LIST) | .build/frece-$(FRECE_VERSION) ; ln -fsrT $| $@; touch $@
.build/frece-$(FRECE_VERSION):
	cargo install --root .build/frece-$(FRECE_VERSION) --path ~/src-rust/frece

RGA_SRC := ~/src-rust/ripgrep-all
RGA_VERSION := $(shell $(call GIT_DESCRIBE,$(RGA_SRC)))

RGA_BINS := rga rga-preproc rga-fzf rga-fzf-open
all: $(RGA_BINS)
$(RGA_BINS) &: $(MAKEFILE_LIST) | .build/rga ; $(foreach BIN,$(RGA_BINS),ln -fsT $|/bin/$(BIN) $(BIN); touch $(BIN);)
.build/rga: $(MAKEFILE_LIST) | .build/rga-$(RGA_VERSION) ; ln -fsrT $| $@; touch $@
.build/rga-$(RGA_VERSION):
	cargo install --root .build/rga-$(RGA_VERSION) --locked --path ~/src-rust/ripgrep-all

.PHONY: gc
## Remove old versions
gc:
	liskin-gc-unlinked $(wildcard .binstall/*) $(wildcard .build/*)
