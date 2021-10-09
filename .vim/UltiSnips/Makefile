.PHONY: all
all:

all: all_spdx.snippets
all_spdx.snippets: all_spdx.snippets.j2 Makefile
	basename -s.txt /usr/share/spdx-licenses/*.txt \
	| jq -R -s 'split("\n") | map(select(. != "")) | {licenses: .}' \
	| j2 -f json $< - > $@

include ~/_gitignore.mk

.DELETE_ON_ERROR:
