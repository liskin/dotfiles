all: $(patsubst %.m4,%,$(wildcard *.m4))

%: %.m4
	m4 $< >$@

.PHONY: all
