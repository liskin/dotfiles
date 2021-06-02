include ~/.xmonad/Makefile

XMONAD_DEPS := xmonad xmonad-contrib
BUILD_DEPS_TARGETS := build
SRC_HASKELL := .

.PHONY: build
build: stack.yaml
	stack build

stack.yaml: ~/src-haskell/stack.yaml Makefile
	yq --yaml-output \
		'def keep: . == "X11" or . == "xmonad" or . == "xmonad-contrib"; .packages |= map(select(keep)) | .flags |= with_entries(select(.key | keep)) ' \
		$< >$@

.DELETE_ON_ERROR:
