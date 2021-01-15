.PHONY: all _phony
all:

define SUBDIR_TEMPLATE
$(1): _phony
	$$(MAKE) -C $$@

$(1)/%: _phony
	$$(MAKE) -C $(1) $$*
endef

SUBDIRS := $(shell git-dotfiles ls-files | sed -n -e 's|/Makefile$$||p')
$(foreach subdir,$(SUBDIRS),$(eval $(call SUBDIR_TEMPLATE,$(subdir))))

all: $(SUBDIRS)
