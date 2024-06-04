.PHONY: all
all:

define INIT_GIT_REPOS :=
$(strip
	apt
	bash-history
	browser
	gsettings
	libvirt
	liferea
)
endef

all: $(INIT_GIT_REPOS)
$(INIT_GIT_REPOS):
	mkdir -p $@
	unset GIT_DIR && git init $@

include ~/_gitignore.mk
