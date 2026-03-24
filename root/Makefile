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
	.bashrc
	.bashrc.d/20_history.sh
	.bashrc.d/30_completion_git.sh
	.bashrc.d/31_completion.sh
	.config/git/config
	.config/nvim/lua/init/vimrc.lua
	.config/tig/config
	.dircolors
	.tmux.conf
	.vim/colors/liskin.vim
	.vimrc
	_gitignore.mk
	_help.mk
	bin/git-repo-name
	bin/liskin-make-gitignore
	bootstrap.sh
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
