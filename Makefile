.PHONY: all _phony
all:

# make git-dotfiles first and restart make
ifeq ($(shell command -v git-dotfiles),)
include bin/.aliases/.done
bin/.aliases/.done:
	$(MAKE) -C bin .aliases/.done
endif

include ~/_help.mk

define SUBDIR_TEMPLATE
$(1): _phony
	$$(MAKE) -C $$@

$(1)\%%: _phony
	$$(MAKE) -C $(1) $$*

help-src: help-src-$(1)
help-src-$(1):
	@echo "##"
	@echo "$(1):"
	@echo "##"
	@echo "$(1)%target:"
endef

SUBDIRS := $(shell git-dotfiles ls-files | sed -n -e 's|/Makefile$$||p')
$(foreach subdir,$(SUBDIRS),$(eval $(call SUBDIR_TEMPLATE,$(subdir))))

SUBDIRS_EXCLUDE := docs/resume-cv .xmonad-testing

## Invoke make for all subdirs with Makefiles
all: $(filter-out $(SUBDIRS_EXCLUDE),$(SUBDIRS))

.local/share/mise: bin/.ext .rustup

## Invoke "make gc" in sub-Makefiles
gc: bin/.ext%gc
gc: src-elixir%gc
gc: .local/share/mise%gc

## Invoke "make update" in sub-Makefiles
update: .local/share/mise%update
update: bin/.ext%update

.PHONY: $(filter bootstrap.sh,$(MAKECMDGOALS))
all: bootstrap.sh
.SKIP_GITIGNORE: bootstrap.sh
##
bootstrap.sh: bin/liskin-include-preproc.py bin/.o
	liskin-include-preproc.py $@
