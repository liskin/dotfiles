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

include ~/_gitignore.mk
