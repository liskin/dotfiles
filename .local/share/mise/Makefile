# dotfiles.git will get messed up otherwise,
# and also some git operations won't work
# (in uv, mise itself seems fine)
undefine GIT_DIR

.PHONY: all
all:
	mise install
	$(MAKE) fixup

.PHONY: fixup
fixup: fixup-bottle-py
fixup: fixup-tilt

# FIXME: workaround for https://github.com/astral-sh/uv/issues/6489
# see https://github.com/bottlepy/bottle/commit/c92516069e664efc9bac3746004402bd6f3cc9ec#r146746445
.PHONY: fixup-bottle-py
fixup-bottle-py:
	find installs -path 'installs/pipx-*/bin/bottle.py' -delete

# let tilt open privileged ports
.PHONY: fixup-tilt
fixup-tilt:
	if bin=$$(mise which tilt) && ! /usr/sbin/setcap -q -v cap_net_bind_service+ep "$$bin"; then \
		sudo setcap cap_net_bind_service+ep "$$bin"; \
	fi

.PHONY: update
## Check for updates
update:
	mise upgrade --bump --interactive

.PHONY: gc
## Delete unused versions of tools
gc:
	mise prune

include ~/_help.mk
