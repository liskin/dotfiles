.PHONY: all
all:

INIT_GIT_REPOS := bash-history browser liferea
all: $(INIT_GIT_REPOS)
$(INIT_GIT_REPOS):
	mkdir -p $@
	unset GIT_DIR && git init $@
