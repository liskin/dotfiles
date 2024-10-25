.PHONY: all
##
all:

.PHONY: gh-codespaces.conf
all: gh-codespaces.conf
##
gh-codespaces.conf:
	gh codespace ssh --config | sed -e 's| -i.*codespaces.auto||; /IdentityFile/d;' >$@
# ^ sed because https://github.com/cli/cli/issues/9817

include ~/_gitignore.mk
include ~/_help.mk

.DELETE_ON_ERROR:

