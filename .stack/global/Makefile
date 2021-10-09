.PHONY:
all:

all: stack.yaml
stack.yaml: ~/src-haskell/stack.yaml Makefile
	yq --yaml-output '{resolver: .resolver, packages: []}' $< >$@

include ~/_gitignore.mk

.DELETE_ON_ERROR:
