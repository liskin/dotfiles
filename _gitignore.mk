all: .gitignore
.SKIP_GITIGNORE: .gitignore
.gitignore: Makefile ~/_gitignore.mk
	liskin-make-gitignore >>$@
	LC_ALL=C sort -u -o $@ $@
.PRECIOUS: .gitignore
