.PHONY: all
##
all:

all: diff-highlight
diff-highlight: $(wildcard /usr/share/doc/git/contrib/diff-highlight/*)
ifneq "$(__bwrapped)" "1"
	__bwrapped=1 bwrap --dev-bind / / --tmpfs /tmp -- $(MAKE) $@
else
	cp -a /usr/share/doc/git/contrib/diff-highlight /tmp/diff-highlight
	$(MAKE) -C /tmp/diff-highlight diff-highlight
	cp /tmp/diff-highlight/diff-highlight $@
endif

all: .aliases/.done
.SKIP_GITIGNORE: .aliases/.done
.aliases/.done: .aliases/.update.sh $(wildcard ~/.bashrc.d/5*_aliases*.sh)
	$<

all: .venv-slack-sdk
.venv-slack-sdk:
	python3 -m venv --clear --system-site-packages --without-pip $@
	$@/bin/python3 -m pip install slack-sdk

.PHONY: $(filter xrandr-smart,$(MAKECMDGOALS))
all: xrandr-smart
.SKIP_GITIGNORE: xrandr-smart
##
xrandr-smart: liskin-include-preproc.py .o
	liskin-include-preproc.py $@

include ~/_gitignore.mk
include ~/_help.mk

.DELETE_ON_ERROR:
