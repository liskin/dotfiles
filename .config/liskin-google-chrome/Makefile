.PHONY: all
all:

ifeq "$(words $(wildcard $(HOME)/.password-store/google/api/chrome-sync.gpg))" "1"
all: env
env: env.j2 Makefile $(HOME)/.password-store/google/api/chrome-sync.gpg
	pass-extract-yaml-j2 google/api/chrome-sync | j2 -f yaml $< - > $@
endif

include ~/_gitignore.mk

.DELETE_ON_ERROR:
