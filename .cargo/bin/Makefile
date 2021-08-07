.PHONY: all
all:

all: dust
dust:
	cargo install du-dust

all: frece
frece:
	cargo install --path ~/src-rust/frece

all: rga rga-preproc
rga rga-preproc &:
	cargo install ripgrep_all

include ~/_gitignore.mk
