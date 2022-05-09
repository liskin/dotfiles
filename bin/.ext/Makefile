.PHONY: all
all:

define GITHUB_FETCH_RELEASE_ASSET
	{ url="$$(gh api --jq '.assets[] | select(.name == "$(strip $(3))") | .browser_download_url' repos/"$(strip $(1))"/releases/$(strip $(2)))" \
		&& : "$${url:?}" \
		&& curl -LSsf "$$url"; }
endef

CURL := curl -LSsf

CARGO_BINSTALL_VERSION := 0.7.0
CTLPTL_VERSION := 0.8.0
DELTA_VERSION := 0.12.1
HADOLINT_VERSION := 2.10.0
HLINT_VERSION := 3.4
HLS_GHC_VERSION := 8.8.4
HLS_VERSION := 1.7.0.0
KIND_VERSION := 0.12.0
LANGUAGETOOL_VERSION := 5.7
PANDOC_VERSION := 2.18
RUST_ANALYZER_VERSION := 2022-05-02
TILT_VERSION := 0.29.0
TZ_VERSION := 0.6.1

all: rust-analyzer
rust-analyzer: | .ext-rust-analyzer ; ln -fsT $| $@
.ext-rust-analyzer: .ext-rust-analyzer-$(RUST_ANALYZER_VERSION) ; ln -fsT $< $@
.ext-rust-analyzer-$(RUST_ANALYZER_VERSION):
	$(call GITHUB_FETCH_RELEASE_ASSET,\
		rust-analyzer/rust-analyzer,\
		tags/$(RUST_ANALYZER_VERSION),\
		rust-analyzer-x86_64-unknown-linux-gnu.gz\
	) | gunzip >$@
	chmod +x $@

all: haskell-language-server-$(HLS_GHC_VERSION) haskell-language-server-wrapper
haskell-language-server-$(HLS_GHC_VERSION) haskell-language-server-wrapper: | .ext-hls ; ln -fsT $|/bin/$@ $@
.ext-hls: .ext-hls-$(HLS_VERSION) ; ln -fsT $< $@
.ext-hls-$(HLS_VERSION): .ext-hls-$(HLS_VERSION).tar.xz
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

.INTERMEDIATE: .ext-hls-$(HLS_VERSION).tar.xz
.ext-hls-$(HLS_VERSION).tar.xz:
	curl -C - -o $@ https://downloads.haskell.org/~hls/haskell-language-server-$(HLS_VERSION)/haskell-language-server-$(HLS_VERSION)-x86_64-linux-deb10.tar.xz

all: tz
tz: | .ext-tz ; ln -fsT $|/$@ $@
.ext-tz: .ext-tz-$(TZ_VERSION) ; ln -fsT $< $@
.ext-tz-$(TZ_VERSION):
	mkdir -p $@
	$(call GITHUB_FETCH_RELEASE_ASSET,\
		oz/tz,\
		tags/v$(TZ_VERSION),\
		tz-$(TZ_VERSION)-linux-amd64.tar.xz\
	) | tar -x -J -C $@ tz

all: delta
delta: | .ext-delta ; ln -fsT $|/$@ $@
.ext-delta: .ext-delta-$(DELTA_VERSION) ; ln -fsT $< $@
.ext-delta-$(DELTA_VERSION):
	mkdir -p $@
	$(call GITHUB_FETCH_RELEASE_ASSET,\
		dandavison/delta,\
		tags/$(DELTA_VERSION),\
		delta-$(DELTA_VERSION)-x86_64-unknown-linux-gnu.tar.gz\
	) | tar -x -z -C $@ --strip-components=1 --wildcards "*/delta"

all: pandoc
pandoc: | .ext-pandoc ; ln -fsT $|/bin/$@ $@
.ext-pandoc: .ext-pandoc-$(PANDOC_VERSION) ; ln -fsT $< $@
.ext-pandoc-$(PANDOC_VERSION):
	mkdir -p $@
	$(call GITHUB_FETCH_RELEASE_ASSET,\
		jgm/pandoc,\
		tags/$(PANDOC_VERSION),\
		pandoc-$(PANDOC_VERSION)-linux-amd64.tar.gz\
	) | tar -x -z -C $@ --strip-components=1

all: hlint
hlint: | .ext-hlint ; ln -fsT $|/$@ $@
.ext-hlint: .ext-hlint-$(HLINT_VERSION) ; ln -fsT $< $@
.ext-hlint-$(HLINT_VERSION):
	mkdir -p $@
	$(call GITHUB_FETCH_RELEASE_ASSET,\
		ndmitchell/hlint,\
		tags/v$(HLINT_VERSION),\
		hlint-$(HLINT_VERSION)-x86_64-linux.tar.gz\
	) | tar -x -z -C $@ --strip-components=1

all: .ext-languagetool
.SKIP_GITIGNORE: .ext-languagetool
.ext-languagetool: .ext-languagetool-$(LANGUAGETOOL_VERSION) ; ln -fsT $< $@
.ext-languagetool-$(LANGUAGETOOL_VERSION): .ext-languagetool-$(LANGUAGETOOL_VERSION).zip
	mkdir -p $@
	unzip -d $@ $<
	mv -T $@/LanguageTool-$(LANGUAGETOOL_VERSION) $@.tmp
	mv -T $@.tmp $@

.INTERMEDIATE: .ext-languagetool-$(LANGUAGETOOL_VERSION).zip
.ext-languagetool-$(LANGUAGETOOL_VERSION).zip:
	curl -C - -o $@ https://languagetool.org/download/LanguageTool-$(LANGUAGETOOL_VERSION).zip

all: hadolint
hadolint: | .ext-hadolint ; ln -fsT $| $@
.ext-hadolint: .ext-hadolint-$(HADOLINT_VERSION) ; ln -fsT $< $@
.ext-hadolint-$(HADOLINT_VERSION):
	$(call GITHUB_FETCH_RELEASE_ASSET,\
		hadolint/hadolint,\
		tags/v$(HADOLINT_VERSION),\
		hadolint-Linux-x86_64\
	) >$@
	chmod +x $@

all: tilt
tilt: | .ext-tilt
	ln -fsT $|/$@ $@
	./$@ analytics opt out
.ext-tilt: .ext-tilt-$(TILT_VERSION) ; ln -fsT $< $@
.ext-tilt-$(TILT_VERSION):
	mkdir -p $@
	$(call GITHUB_FETCH_RELEASE_ASSET,\
		tilt-dev/tilt,\
		tags/v$(TILT_VERSION),\
		tilt.$(TILT_VERSION).linux.x86_64.tar.gz\
	) | tar -x -z -C $@ tilt

all: ctlptl
ctlptl: | .ext-ctlptl
	ln -fsT $|/$@ $@
	./$@ analytics opt out
.ext-ctlptl: .ext-ctlptl-$(CTLPTL_VERSION) ; ln -fsT $< $@
.ext-ctlptl-$(CTLPTL_VERSION):
	mkdir -p $@
	$(call GITHUB_FETCH_RELEASE_ASSET,\
		tilt-dev/ctlptl,\
		tags/v$(CTLPTL_VERSION),\
		ctlptl.$(CTLPTL_VERSION).linux.x86_64.tar.gz\
	) | tar -x -z -C $@ ctlptl

all: kind
kind: | .ext-kind ; ln -fsT $| $@
.ext-kind: .ext-kind-$(KIND_VERSION) ; ln -fsT $< $@
.ext-kind-$(KIND_VERSION):
	$(call GITHUB_FETCH_RELEASE_ASSET,\
		kubernetes-sigs/kind,\
		tags/v$(KIND_VERSION),\
		kind-linux-amd64\
	) >$@
	chmod +x $@

all: cargo-binstall
cargo-binstall: | .ext-cargo-binstall ; ln -fsT $|/$@ $@
.ext-cargo-binstall: .ext-cargo-binstall-$(CARGO_BINSTALL_VERSION) ; ln -fsT $< $@
.ext-cargo-binstall-$(CARGO_BINSTALL_VERSION):
	mkdir -p $@
	$(CURL) https://github.com/ryankurte/cargo-binstall/releases/download/v$(CARGO_BINSTALL_VERSION)/cargo-binstall-x86_64-unknown-linux-gnu.tgz \
		| tar -x -z -C $@ cargo-binstall

.PHONY: gc
gc:
	liskin-gc-unlinked $(wildcard .ext-*)

.PHONY: clean
clean:
	$(RM) -r $(wildcard .ext-*)

include ~/_gitignore.mk

.DELETE_ON_ERROR:
