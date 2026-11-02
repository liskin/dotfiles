# dotfiles.git will get messed up otherwise,
# and also some git operations won't work
# (in uv, mise itself seems fine)
undefine GIT_DIR

.PHONY: all
all:
	mise install

.PHONY: update
## Check for updates
update:
	mise upgrade --bump --interactive

.PHONY: gc
## Delete unused versions of tools
gc:
	mise prune

include ~/_help.mk
