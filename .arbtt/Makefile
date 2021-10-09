.PHONY:
all:

all: categorize.cfg
categorize.cfg: categorize.m4 $(wildcard categorize*.m4)
	m4 $< > $@

include ~/_gitignore.mk

.DELETE_ON_ERROR:
