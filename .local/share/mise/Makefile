.PHONY: all
all: shims/cargo-binstall
	mise install

# FIXME: workaround for https://github.com/jdx/mise/issues/2260
shims/cargo-binstall:
	mise install ubi:cargo-bins/cargo-binstall

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
