all: .gitignore
.SKIP_GITIGNORE: .gitignore
.gitignore: Makefile $(shell realpath --relative-to=. ~/_gitignore.mk ~/bin/liskin-make-gitignore)
	liskin-make-gitignore >>$@
	LC_ALL=C sort -u -o $@ $@
.PRECIOUS: .gitignore
