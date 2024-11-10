.PHONY: all
##
all:

all: completions/git-gh-codespace-rsync
completions/git-gh-codespace-rsync:
	usage generate completion bash $(notdir $@) --usage-cmd "$(notdir $@) --usage" >$@

include ~/_gitignore.mk
include ~/_help.mk

.DELETE_ON_ERROR:
