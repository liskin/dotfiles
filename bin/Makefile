.PHONY: all
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

all: rust-analyzer
rust-analyzer:
	url="$$(gh api --jq '.assets[] | select(.name == "rust-analyzer-x86_64-unknown-linux-gnu.gz") | .browser_download_url' repos/rust-analyzer/rust-analyzer/releases/latest)" \
		&& : "$${url:?}" \
		&& curl -LSsf "$$url" | gunzip >$@ \
		&& chmod +x $@

all: .aliases/.done
.SKIP_GITIGNORE: .aliases/.done
.aliases/.done: $(wildcard ~/.bashrc.d/5*_aliases*.sh)
	.aliases/.update.sh

include ~/_gitignore.mk
