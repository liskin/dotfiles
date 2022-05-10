.PHONY: all
all:

DUST_VERSION := 0.8.0
CARGO_CACHE_VERSION := 0.8.2
CARGO_WATCH_VERSION := 8.1.1

CARGO_BINSTALL := cargo binstall --no-confirm --install-path .binstall

.binstall:
	mkdir -p $@

all: dust
dust: | .binstall/dust ; ln -fsT $| $@
.binstall/dust: .binstall/dust-v$(DUST_VERSION)
.binstall/dust-v$(DUST_VERSION): | .binstall
	$(CARGO_BINSTALL) --version =$(DUST_VERSION) du-dust

all: cargo-watch
cargo-watch: | .binstall/cargo-watch ; ln -fsT $| $@
.binstall/cargo-watch: .binstall/cargo-watch-v$(CARGO_WATCH_VERSION)
.binstall/cargo-watch-v$(CARGO_WATCH_VERSION): | .binstall
	$(CARGO_BINSTALL) --version =$(CARGO_WATCH_VERSION) cargo-watch

all: cargo-cache
cargo-cache: | .binstall/cargo-cache ; ln -fsT $| $@
.binstall/cargo-cache: .binstall/cargo-cache-v$(CARGO_CACHE_VERSION)
.binstall/cargo-cache-v$(CARGO_CACHE_VERSION): | .binstall
	$(CARGO_BINSTALL) --version =$(CARGO_CACHE_VERSION) cargo-cache

all: frece
frece:
	cargo install --path ~/src-rust/frece

all: rga rga-preproc rga-fzf rga-fzf-open
rga rga-preproc rga-fzf rga-fzf-open &:
	cargo install --locked --path ~/src-rust/ripgrep-all

.PHONY: gc
gc:
	liskin-gc-unlinked $(wildcard .binstall/*) $(wildcard .build/*)

include ~/_gitignore.mk
