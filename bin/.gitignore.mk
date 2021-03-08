all: .gitignore
.gitignore: Makefile
	grep -P -o '^all: \K.*' $< \
		| xargs -n1 \
		| { grep -v -F -e / || :; } \
		| { grep -v -F -x -e .gitignore || :; } \
		| sed 's|^|/|' \
		>> $@
	LC_ALL=C sort -u -o $@ $@
