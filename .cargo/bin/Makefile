.PHONY: all
all:

include ~/_git.mk
include ~/_github.mk
include ~/_gitignore.mk
include ~/_help.mk

HOSTNAME := $(shell hostname)

CARGO_CACHE_VERSION := 0.8.3
CARGO_EDIT_VERSION := v0.11.9
CARGO_WATCH_VERSION := v8.4.0
DIESEL_CLI_14_VERSION := v1.4.1
DIESEL_CLI_VERSION := v2.0.1
DUST_VERSION := v0.8.5
WATCHEXEC_VERSION := v1.21.1

CARGO_CACHE_GITHUB_LASTTAG := matthiaskrgr/cargo-cache
CARGO_EDIT_GITHUB := killercup/cargo-edit
CARGO_WATCH_GITHUB := watchexec/cargo-watch
DIESEL_CLI_GITHUB := diesel-rs/diesel
DUST_GITHUB := bootandy/dust
WATCHEXEC_GITHUB := watchexec/watchexec

.PHONY: update
## Check for updates
update: update-github

CARGO_BINSTALL_COMPILE := cargo binstall --no-confirm
CARGO_BINSTALL := $(CARGO_BINSTALL_COMPILE) --disable-strategies compile

.binstall:
	mkdir -p $@

all: dust
dust: | .binstall/dust ; ln -fsT $|/bin/$@ $@
.binstall/dust: .binstall/dust-$(DUST_VERSION) ; ln -fsrT $< $@
.binstall/dust-$(DUST_VERSION): | .binstall
	$(CARGO_BINSTALL) --root $@ --version =$(DUST_VERSION:v%=%) du-dust

all: cargo-watch
cargo-watch: | .binstall/cargo-watch ; ln -fsT $|/bin/$@ $@
.binstall/cargo-watch: .binstall/cargo-watch-$(CARGO_WATCH_VERSION) ; ln -fsrT $< $@
.binstall/cargo-watch-$(CARGO_WATCH_VERSION): | .binstall
	$(CARGO_BINSTALL) --root $@ --version =$(CARGO_WATCH_VERSION:v%=%) cargo-watch

all: cargo-cache
cargo-cache: | .binstall/cargo-cache ; ln -fsT $|/bin/$@ $@
.binstall/cargo-cache: .binstall/cargo-cache-v$(CARGO_CACHE_VERSION) ; ln -fsrT $< $@
.binstall/cargo-cache-v$(CARGO_CACHE_VERSION): | .binstall
	$(CARGO_BINSTALL) --root $@ --version =$(CARGO_CACHE_VERSION) cargo-cache

all: cargo-add cargo-rm cargo-set-version cargo-upgrade
cargo-add cargo-rm cargo-set-version cargo-upgrade: | .binstall/cargo-upgrade ; ln -fsT $|/bin/$@ $@
.binstall/cargo-upgrade: .binstall/cargo-upgrade-$(CARGO_EDIT_VERSION) ; ln -fsrT $< $@
.binstall/cargo-upgrade-$(CARGO_EDIT_VERSION): | .binstall
	$(CARGO_BINSTALL) --root $@ --version =$(CARGO_EDIT_VERSION:v%=%) cargo-edit

all: watchexec
watchexec: | .binstall/watchexec ; ln -fsT $|/bin/$@ $@
.binstall/watchexec: .binstall/watchexec-$(WATCHEXEC_VERSION) ; ln -fsrT $< $@
.binstall/watchexec-$(WATCHEXEC_VERSION): | .binstall
	$(CARGO_BINSTALL) --root $@ --version =$(WATCHEXEC_VERSION:v%=%) watchexec-cli

all: $(if $(filter pftp.%,$(HOSTNAME)),diesel)
diesel: | .binstall/diesel ; ln -fsT $|/bin/$@ $@
.binstall/diesel: .binstall/diesel-$(DIESEL_CLI_VERSION) ; ln -fsrT $< $@
.binstall/diesel-$(DIESEL_CLI_VERSION): | .binstall
	$(CARGO_BINSTALL_COMPILE) --root $@ --version =$(DIESEL_CLI_VERSION:v%=%) diesel_cli

all: $(if $(filter pftp.%,$(HOSTNAME)),diesel1.4)
diesel1.4: | .binstall/diesel1.4 ; ln -fsT $|/bin/diesel $@
.binstall/diesel1.4: .binstall/diesel1.4-$(DIESEL_CLI_14_VERSION) ; ln -fsrT $< $@
.binstall/diesel1.4-$(DIESEL_CLI_14_VERSION): | .binstall
	$(CARGO_BINSTALL_COMPILE) --root $@ --version =$(DIESEL_CLI_14_VERSION:v%=%) diesel_cli

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
$(RGA_BINS): $(MAKEFILE_LIST) | .build/rga ; ln -fsT $|/bin/$@ $@; touch $@
.build/rga: $(MAKEFILE_LIST) | .build/rga-$(RGA_VERSION) ; ln -fsrT $| $@; touch $@
.build/rga-$(RGA_VERSION):
	cargo install --root .build/rga-$(RGA_VERSION) --locked --path ~/src-rust/ripgrep-all

.PHONY: gc
## Remove old versions
gc:
	liskin-gc-unlinked $(wildcard .binstall/*) $(wildcard .build/*)
