.PHONY: all
all:

all: muttrc-lists
muttrc-lists: $(wildcard ~/.cache/mu/xapian/*)
	mu find --fields=l body:mailman body:listinfo body:options \
		| xargs grep -P -i -o -h '^errors-to:\s+\K.*' \
		| sed -e 's/-bounces.*@/@/; s/^/lists /; s/\./\\./' \
		| sort -u \
		> $@

ifeq "$(words $(wildcard $(HOME)/.password-store/nomi/imap.gpg))" "1"
all: muttrc-nomi
muttrc-nomi: muttrc-nomi.j2 Makefile $(HOME)/.password-store/nomi/imap.gpg
	pass-extract-yaml-j2 nomi/imap | j2 -f yaml $< - > $@
endif

include ~/_gitignore.mk

.DELETE_ON_ERROR:
