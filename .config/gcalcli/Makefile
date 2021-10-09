.PHONY: all
all:

all: .gcalclirc
.gcalclirc: .gcalclirc.j2 Makefile $(HOME)/.password-store/google/liskni.si@gmail.com/calendar.gpg
	pass-extract-yaml-j2 google/liskni.si@gmail.com/calendar | j2 -f yaml $< - > $@

include ~/_gitignore.mk

.DELETE_ON_ERROR:
