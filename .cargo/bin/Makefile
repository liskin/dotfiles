.PHONY: all
all:

all: dust
dust:
	cargo install du-dust

all: frece
frece:
	cargo install --path ~/src-rust/frece

all: rga rga-preproc rga-fzf rga-fzf-open
rga rga-preproc rga-fzf rga-fzf-open &:
	cargo install --locked --path ~/src-rust/ripgrep-all

all: cargo-watch
cargo-watch:
	cargo binstall --no-confirm --version =8.1.1 cargo-watch

include ~/_gitignore.mk
