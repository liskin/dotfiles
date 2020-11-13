.PHONY: all
all: diff-highlight

diff-highlight: $(wildcard /usr/share/doc/git/contrib/diff-highlight/*)
ifneq "$(__bwrapped)" "1"
	__bwrapped=1 bwrap --dev-bind / / --tmpfs /tmp -- $(MAKE) $@
else
	cp -a /usr/share/doc/git/contrib/diff-highlight /tmp/diff-highlight
	$(MAKE) -C /tmp/diff-highlight diff-highlight
	cp /tmp/diff-highlight/diff-highlight $@
endif
