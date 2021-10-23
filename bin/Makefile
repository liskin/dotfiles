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

define GITHUB_FETCH_RELEASE_ASSET
	{ url="$$(gh api --jq '.assets[] | select(.name == "$(strip $(3))") | .browser_download_url' repos/"$(strip $(1))"/releases/$(strip $(2)))" \
		&& : "$${url:?}" \
		&& curl -LSsf "$$url"; }
endef

all: rust-analyzer
rust-analyzer:
	$(call GITHUB_FETCH_RELEASE_ASSET,\
		rust-analyzer/rust-analyzer,\
		latest,\
		rust-analyzer-x86_64-unknown-linux-gnu.gz\
	) | gunzip >$@
	chmod +x $@

all: haskell-language-server-wrapper
haskell-language-server-wrapper:
	$(call GITHUB_FETCH_RELEASE_ASSET,\
		haskell/haskell-language-server,\
		latest,\
		haskell-language-server-wrapper-Linux.gz\
	) | gunzip >$@
	chmod +x $@

all: haskell-language-server-8.8.4
haskell-language-server-%:
	$(call GITHUB_FETCH_RELEASE_ASSET,\
		haskell/haskell-language-server,\
		latest,\
		haskell-language-server-Linux-$*.gz\
	) | gunzip >$@
	chmod +x $@

all: .pandoc
.pandoc: PANDOC_VERSION=2.14.2
.pandoc:
	mkdir -p $@
	$(call GITHUB_FETCH_RELEASE_ASSET,\
		jgm/pandoc,\
		tags/$(PANDOC_VERSION),\
		pandoc-$(PANDOC_VERSION)-linux-amd64.tar.gz\
	) | tar -x -z -C $@ --strip-components=1

all: .aliases/.done
.SKIP_GITIGNORE: .aliases/.done
.aliases/.done: $(wildcard ~/.bashrc.d/5*_aliases*.sh)
	.aliases/.update.sh

include ~/_gitignore.mk

.DELETE_ON_ERROR:
