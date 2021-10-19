include ~/.xmonad/Makefile

XMONAD_DEPS := X11 xmonad xmonad-contrib
BUILD_DEPS_TARGETS := build
BUILD_DOCS_TARGETS := build-haddock
SRC_HASKELL := .
STACK_BUILDFLAGS :=

.PHONY: build
build: stack.yaml hie.yaml
	stack build $(STACK_BUILDFLAGS) $(BUILD_TARGETS)

.PHONY: build-haddock
build-haddock: STACK_BUILDFLAGS=--haddock --no-haddock-deps --no-haddock-hyperlink-source --haddock-arguments --optghc=-dynamic
build-haddock: build

stack.yaml: ~/src-haskell/stack.yaml Makefile | $(XMONAD_DEPS)
	yq --yaml-output \
		'def keep: IN("$(XMONAD_DEPS)" | splits(" ")); .packages |= map(select(keep)) | .flags |= with_entries(select(.key | keep)) ' \
		$< >$@

hie.yaml: stack.yaml
	gen-hie >$@

.PHONY: clone-deps
## clone (worktree add) deps
clone-deps: $(XMONAD_DEPS)

GIT_WORKTREE_ADD = \
	unset GIT_DIR && \
	git -C ~/src-haskell/$(1) worktree prune && \
	git -C ~/src-haskell/$(1) worktree add $(CURDIR)/$(1) --detach origin/master
$(XMONAD_DEPS):
	$(call GIT_WORKTREE_ADD,$@)

.PHONY: clean
##
clean:
	$(RM) -r \
		stack.yaml stack.yaml.lock hie.yaml \
		.build .stack-work \
		$(XMONAD_DEPS) $(XMONAD_BINARY)

.sub/xmonad-: # tab-completion
## prepare sub-workspace
.sub/xmonad-%:
	mkdir -p $@
	ln -fsr Makefile $@/Makefile
	git-dotfiles show @:.xmonad-testing/xmonad.hs >$@/xmonad.hs
	make -C $@

.DELETE_ON_ERROR:
