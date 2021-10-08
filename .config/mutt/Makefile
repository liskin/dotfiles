.PHONY: all
all:

all: muttrc-lists
muttrc-lists: $(wildcard ~/.cache/mu/xapian/*)
	mu find --fields=l body:mailman body:listinfo body:options \
		| xargs grep -P -i -o -h '^errors-to:\s+\K.*' \
		| sed -e 's/-bounces.*@/@/; s/^/lists /; s/\./\\./' \
		| sort -u \
		> $@

all: muttrc-nomi
muttrc-nomi: muttrc-nomi.j2 Makefile $(HOME)/.password-store/nomi/imap.gpg
	pass-extract-yaml nomi/imap | j2 -f yaml $< - > $@
