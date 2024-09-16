# dotfiles.git will get messed up otherwise,
# and also some git operations won't work
# (in uv, mise itself seems fine)
undefine GIT_DIR

.PHONY: all
all: shims/cargo-binstall shims/uv
	mise install
	$(MAKE) fixup

# FIXME: workaround for https://github.com/jdx/mise/issues/2260
shims/cargo-binstall:
	mise install ubi:cargo-bins/cargo-binstall
shims/uv:
	mise install ubi:astral-sh/uv

.PHONY: fixup
fixup: fixup-bottle-py

# FIXME: workaround for https://github.com/astral-sh/uv/issues/6489
# see https://github.com/bottlepy/bottle/commit/c92516069e664efc9bac3746004402bd6f3cc9ec#r146746445
.PHONY: fixup-bottle-py
fixup-bottle-py:
	find installs -path 'installs/pipx-*/bin/bottle.py' -delete

.PHONY: update
## Check for updates
update:
	mise ls --global --json | jq -r 'keys[] | "\(.)@latest"' | xargs mise upgrade --dry-run
	# mise use --global --pin …

.PHONY: gc
## Delete unused versions of tools
gc:
	mise prune

include ~/_help.mk
