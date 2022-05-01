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

HLS_VERSION := 1.7.0.0
HLS_GHC_VERSION := 8.8.4
HLINT_VERSION := 3.4
TZ_VERSION := 0.6.1
DELTA_VERSION := 0.11.3
PANDOC_VERSION := 2.16.1
LANGUAGETOOL_VERSION := 5.5

all: rust-analyzer
rust-analyzer:
	$(call GITHUB_FETCH_RELEASE_ASSET,\
		rust-analyzer/rust-analyzer,\
		latest,\
		rust-analyzer-x86_64-unknown-linux-gnu.gz\
	) | gunzip >$@
	chmod +x $@

all: .ext-hls
.ext-hls: .ext-hls.tar.xz
	mkdir -p $@
	mkdir -p $@/.tmp
	tar -C $@/.tmp -xaf $< --strip-components=1 --wildcards \
		"*"/GNUmakefile \
		"*"/scripts \
		"*"/haskell-language-server-$(HLS_GHC_VERSION).in \
		"*"/bin/haskell-language-server-wrapper \
		"*"/bin/haskell-language-server-$(HLS_GHC_VERSION) \
		"*"/lib/$(HLS_GHC_VERSION)
	make -C $@/.tmp PREFIX=$(CURDIR)/$@
	$(RM) -r $@/.tmp

.INTERMEDIATE: .ext-hls.tar.xz
.ext-hls.tar.xz:
	curl -C - -o $@ https://downloads.haskell.org/~hls/haskell-language-server-$(HLS_VERSION)/haskell-language-server-$(HLS_VERSION)-x86_64-linux-deb10.tar.xz

all: tz
tz:
	$(call GITHUB_FETCH_RELEASE_ASSET,\
		oz/tz,\
		tags/v$(TZ_VERSION),\
		tz-$(TZ_VERSION)-linux-amd64.tar.xz\
	) | tar -x -J tz

all: delta
delta:
	$(call GITHUB_FETCH_RELEASE_ASSET,\
		dandavison/delta,\
		tags/$(DELTA_VERSION),\
		delta-$(DELTA_VERSION)-x86_64-unknown-linux-gnu.tar.gz\
	) | tar -x -z --strip-components=1 --wildcards "*/delta"

all: .ext-pandoc
.ext-pandoc:
	mkdir -p $@
	$(call GITHUB_FETCH_RELEASE_ASSET,\
		jgm/pandoc,\
		tags/$(PANDOC_VERSION),\
		pandoc-$(PANDOC_VERSION)-linux-amd64.tar.gz\
	) | tar -x -z -C $@ --strip-components=1

all: .ext-hlint
.ext-hlint:
	mkdir -p $@
	$(call GITHUB_FETCH_RELEASE_ASSET,\
		ndmitchell/hlint,\
		tags/v$(HLINT_VERSION),\
		hlint-$(HLINT_VERSION)-x86_64-linux.tar.gz\
	) | tar -x -z -C $@ --strip-components=1

all: .ext-languagetool
.ext-languagetool: .ext-languagetool.zip
	mkdir -p $@
	unzip -d $@ .ext-languagetool.zip
	mv -T $@/LanguageTool-$(LANGUAGETOOL_VERSION) $@.tmp
	mv -T $@.tmp $@

.INTERMEDIATE: .ext-languagetool.zip
.ext-languagetool.zip:
	curl -C - -o $@ https://languagetool.org/download/LanguageTool-$(LANGUAGETOOL_VERSION).zip

all: .venv-pulsectl
.venv-pulsectl:
	python3 -m venv --system-site-packages --without-pip $@
	$@/bin/python3 -m pip install pulsectl-asyncio

all: .aliases/.done
.SKIP_GITIGNORE: .aliases/.done
.aliases/.done: $(wildcard ~/.bashrc.d/5*_aliases*.sh)
	.aliases/.update.sh

include ~/_gitignore.mk

.DELETE_ON_ERROR:
