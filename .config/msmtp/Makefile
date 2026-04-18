.PHONY: all
all:

ifeq "$(words $(wildcard $(HOME)/.password-store/app-pass/fastmail-imap.gpg))" "1"
all: config
config: config.j2 Makefile $(HOME)/.password-store/app-pass/fastmail-imap.gpg
	pass-extract-yaml-j2 app-pass/fastmail-imap | j2 -f yaml $< - > $@
	chmod 0600 $@
endif

include ~/_gitignore.mk

.DELETE_ON_ERROR:
