.PHONY: all
##
all:

.PHONY: gh-codespaces.conf
all: gh-codespaces.conf
##
gh-codespaces.conf:
	-gh codespace ssh --config >$@

include ~/_gitignore.mk
include ~/_help.mk

.DELETE_ON_ERROR:
