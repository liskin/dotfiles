.PHONY: all
all:

all: dust
dust:
	cargo install du-dust

all: frece
frece:
	cargo install --git https://github.com/YodaEmbedding/frece

all: rga rga-preproc
rga rga-preproc &:
	cargo install ripgrep_all

include ~/.gitignore.mk
