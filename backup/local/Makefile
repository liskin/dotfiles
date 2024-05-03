.PHONY: all
all:

INIT_GIT_REPOS := bash-history browser liferea libvirt gsettings
all: $(INIT_GIT_REPOS)
$(INIT_GIT_REPOS):
	mkdir -p $@
	unset GIT_DIR && git init $@

include ~/_gitignore.mk
