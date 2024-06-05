.PHONY: all _phony
##
all:

include _help.mk
include _gitignore.mk

.PHONY: homefiles
## get dotfiles from the home branch (make -B homefiles)
homefiles:

define HOMEFILE_TEMPLATE
.PHONY: $$(if $$(filter homefiles,$$(MAKECMDGOALS)),$(1))
homefiles: $(1)
all: $(1)
$(1):
	git-dotfiles-checkout origin/home $$@
endef

define HOMEFILES
$(strip
	.config/git/config
	.config/tig/config
	.tmux.conf
	.vim/colors/liskin.vim
	.vimrc
	_gitignore.mk
	_help.mk
	bin/liskin-make-gitignore
)
endef
$(foreach homefile,$(HOMEFILES),$(eval $(call HOMEFILE_TEMPLATE,$(homefile))))

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

SUBDIRS_EXCLUDE :=
SUBDIRS := $(shell git-dotfiles ls-files | sed -n -e 's|/Makefile$$||p')
all: $(filter-out $(SUBDIRS_EXCLUDE),$(SUBDIRS))
$(foreach subdir,$(SUBDIRS),$(eval $(call SUBDIR_TEMPLATE,$(subdir))))
