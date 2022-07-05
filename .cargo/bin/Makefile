.PHONY: all
all:

CARGO_CACHE_VERSION := 0.8.2
CARGO_WATCH_VERSION := v8.1.1
DUST_VERSION := v0.8.0
FCLONES_VERSION := v0.26.0

CARGO_CACHE_GITHUB_LASTTAG := matthiaskrgr/cargo-cache
CARGO_WATCH_GITHUB := watchexec/cargo-watch
DUST_GITHUB := bootandy/dust
FCLONES_GITHUB := pkolaczk/fclones

.PHONY: update
## Check for updates
update: update-github

CARGO_BINSTALL := cargo binstall --no-confirm --install-path .binstall

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

all: fclones
fclones: | .build/fclones ; ln -fsT $|/bin/$@ $@
.build/fclones: .build/fclones-$(FCLONES_VERSION) ; ln -fsrT $< $@
.build/fclones-$(FCLONES_VERSION):
	cargo install --root .build/fclones-$(FCLONES_VERSION) --version =$(FCLONES_VERSION:v%=%) --locked fclones

all: frece
frece:
	cargo install --path ~/src-rust/frece

all: rga rga-preproc rga-fzf rga-fzf-open
rga rga-preproc rga-fzf rga-fzf-open &:
	cargo install --locked --path ~/src-rust/ripgrep-all

.PHONY: gc
## Remove old versions
gc:
	liskin-gc-unlinked $(wildcard .binstall/*) $(wildcard .build/*)

include ~/_github.mk
include ~/_gitignore.mk
include ~/_help.mk
