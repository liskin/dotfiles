.PHONY:
all:

all: redshift.conf
redshift.conf: redshift.conf.m4 redshift.conf.sh
	./redshift.conf.sh $< >$@

include ~/_gitignore.mk

.DELETE_ON_ERROR:
