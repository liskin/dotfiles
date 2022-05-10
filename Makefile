.PHONY: all _phony
all:

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

.cargo/bin: bin/.ext
