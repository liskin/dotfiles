.PHONY: all
all:

ifeq "$(words $(wildcard $(HOME)/.password-store/nomi/segoe.gpg))" "1"
all: o365-fonts.user.css
o365-fonts.user.css: o365-fonts.user.css.j2 $(HOME)/.password-store/nomi/segoe.gpg
	pass-extract-yaml-j2 nomi/segoe | j2 -f yaml $< - > $@
endif

include ~/_gitignore.mk

.DELETE_ON_ERROR:
