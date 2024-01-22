.PHONY: all
all:

include ~/_git.mk
include ~/_github.mk
include ~/_gitignore.mk
include ~/_help.mk

HOSTNAME := $(shell hostname)

AST_GREP_VERSION := 0.17.1
CARGO_AUDIT_VERSION := v0.17.6
CARGO_CACHE_VERSION := 0.8.3
CARGO_EDIT_VERSION := v0.12.0
CARGO_WATCH_VERSION := v8.4.0
DIESEL_CLI_14_VERSION := v1.4.1
DIESEL_CLI_VERSION := v2.1.0
DUST_VERSION := v0.8.6
TOKIO_CONSOLE_VERSION := tokio-console-v0.1.9
WATCHEXEC_VERSION := v1.22.3

AST_GREP_GITHUB := ast-grep/ast-grep
CARGO_CACHE_GITHUB_LASTTAG := matthiaskrgr/cargo-cache
CARGO_EDIT_GITHUB := killercup/cargo-edit
CARGO_WATCH_GITHUB := watchexec/cargo-watch
DIESEL_CLI_GITHUB := diesel-rs/diesel
DUST_GITHUB := bootandy/dust
TOKIO_CONSOLE_GITHUB := tokio-rs/console
WATCHEXEC_GITHUB := watchexec/watchexec

.PHONY: update
## Check for updates
update: update-github
update: update-cargo-audit

.PHONY: update-cargo-audit
update-cargo-audit:
	@{ \
		v="$$(gh api --jq '.[].ref' repos/rustsec/rustsec/git/refs/tag | sed -ne 's|refs/tags/cargo-audit/||p' | sort --version-sort | tail -1)"; \
		if [ "$$v" != "$(CARGO_AUDIT_VERSION)" ]; then echo "CARGO_AUDIT_VERSION := $$v"; fi; \
	}

CARGO_BINSTALL_COMPILE := cargo binstall --no-confirm
CARGO_BINSTALL := $(CARGO_BINSTALL_COMPILE) --disable-strategies compile

.binstall:
	mkdir -p $@

all: dust
dust: | .binstall/dust ; ln -fsT $|/bin/$@ $@
.binstall/dust: .binstall/dust-$(DUST_VERSION) ; ln -fsrT $< $@
.binstall/dust-$(DUST_VERSION): | .binstall
	$(CARGO_BINSTALL) --root $@ --version =$(DUST_VERSION:v%=%) du-dust

all: cargo-audit
cargo-audit: | .binstall/cargo-audit ; ln -fsT $|/bin/$@ $@
.binstall/cargo-audit: .binstall/cargo-audit-$(CARGO_AUDIT_VERSION) ; ln -fsrT $< $@
.binstall/cargo-audit-$(CARGO_AUDIT_VERSION): | .binstall
	$(CARGO_BINSTALL) --root $@ --version =$(CARGO_AUDIT_VERSION:v%=%) cargo-audit

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
cargo-add cargo-rm cargo-set-version cargo-upgrade: | .binstall/cargo-edit ; ln -fsT $|/bin/$@ $@
.binstall/cargo-edit: .binstall/cargo-edit-$(CARGO_EDIT_VERSION) ; ln -fsrT $< $@
.binstall/cargo-edit-$(CARGO_EDIT_VERSION): | .binstall
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

all: tokio-console
tokio-console: | .binstall/tokio-console ; ln -fsT $|/bin/$@ $@
.binstall/tokio-console: .binstall/tokio-console-$(TOKIO_CONSOLE_VERSION) ; ln -fsrT $< $@
.binstall/tokio-console-$(TOKIO_CONSOLE_VERSION): | .binstall
	$(CARGO_BINSTALL) --root $@ --version =$(TOKIO_CONSOLE_VERSION:tokio-console-v%=%) tokio-console

all: ast-grep sg
ast-grep sg: | .binstall/ast-grep ; ln -fsT $|/bin/$@ $@
.binstall/ast-grep: .binstall/ast-grep-$(AST_GREP_VERSION) ; ln -fsrT $< $@
.binstall/ast-grep-$(AST_GREP_VERSION): | .binstall
	$(CARGO_BINSTALL) --root $@ --version =$(AST_GREP_VERSION:v%=%) ast-grep

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

LIVEBUDSCLI_SRC := ~/src-rust/LiveBudsCli
LIVEBUDSCLI_VERSION := $(shell $(call GIT_DESCRIBE,$(LIVEBUDSCLI_SRC)))

all: earbuds
earbuds: $(MAKEFILE_LIST) | .build/earbuds ; ln -fsT $|/bin/$@ $@; touch $@
.build/earbuds: $(MAKEFILE_LIST) | .build/earbuds-$(LIVEBUDSCLI_VERSION) ; ln -fsrT $| $@; touch $@
.build/earbuds-$(LIVEBUDSCLI_VERSION):
	cargo install --root .build/earbuds-$(LIVEBUDSCLI_VERSION) --path ~/src-rust/LiveBudsCli

.PHONY: gc
## Remove old versions
gc:
	liskin-gc-unlinked $(wildcard .binstall/*) $(wildcard .build/*)
