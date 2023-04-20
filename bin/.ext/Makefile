.PHONY: all
all:

CURL := curl -LSsf

BUILDIFIER_VERSION := 6.1.0
CARGO_AUDIT_VERSION := v0.17.5
CARGO_BINSTALL_VERSION := v0.22.0
CTLPTL_VERSION := v0.8.18
DELTA_VERSION := 0.15.1
DIVE_VERSION := v0.10.0
DOCKER_BUILDX_VERSION := v0.10.4
DOCKER_COMPOSE_VERSION := v2.17.3
EVCXR_VERSION := v0.14.2
FCLONES_VERSION := v0.30.0
FITSDK_VERSION := 21.84.00
GLAB_VERSION := v1.28.0
HADOLINT_VERSION := v2.12.0
HLINT_VERSION := v3.5
HLS_GHC_VERSION := 8.8.4
HLS_VERSION := 1.8.0.0
KIND_VERSION := v0.17.0
LANGUAGETOOL_VERSION := v6.0
PANDOC_VERSION := 2.18
RUSTUP_VERSION := 1.25.2
RUST_ANALYZER_VERSION := 2023-04-17
SCCACHE_VERSION := v0.4.1
TILT_VERSION := v0.32.2
TZ_VERSION := v0.6.3

BUILDIFIER_GITHUB := bazelbuild/buildtools
CARGO_BINSTALL_GITHUB := cargo-bins/cargo-binstall
CTLPTL_GITHUB := tilt-dev/ctlptl
DELTA_GITHUB := dandavison/delta
DIVE_GITHUB := wagoodman/dive
DOCKER_BUILDX_GITHUB := docker/buildx
DOCKER_COMPOSE_GITHUB := docker/compose
EVCXR_GITHUB := google/evcxr
FCLONES_GITHUB := pkolaczk/fclones
GLAB_GITLAB := gitlab-org/cli
HADOLINT_GITHUB := hadolint/hadolint
HLINT_GITHUB := ndmitchell/hlint
HLS_GITHUB := haskell/haskell-language-server
KIND_GITHUB := kubernetes-sigs/kind
LANGUAGETOOL_GITHUB_LASTTAG := languagetool-org/languagetool
PANDOC_GITHUB := jgm/pandoc
RUST_ANALYZER_GITHUB := rust-analyzer/rust-analyzer
SCCACHE_GITHUB := mozilla/sccache
TILT_GITHUB := tilt-dev/tilt
TZ_GITHUB := oz/tz

.PHONY: update
## Check for updates
update: update-github
update: update-rustup
update: update-cargo-audit
update: update-gitlab

.PHONY: update-rustup
update-rustup:
	@{ \
		v="$$($(CURL) https://static.rust-lang.org/rustup/release-stable.toml | tomlq -r .version)"; \
		if [ "$$v" != "$(RUSTUP_VERSION)" ]; then echo "RUSTUP_VERSION := $$v"; fi; \
	}

.PHONY: update-cargo-audit
update-cargo-audit:
	@{ \
		v="$$(gh api --jq '.[].ref' repos/rustsec/rustsec/git/refs/tag | sed -ne 's|refs/tags/cargo-audit/||p' | sort --version-sort | tail -1)"; \
		if [ "$$v" != "$(CARGO_AUDIT_VERSION)" ]; then echo "CARGO_AUDIT_VERSION := $$v"; fi; \
	}

all: rust-analyzer
rust-analyzer: | .ext-rust-analyzer ; ln -fsT $| $@
.ext-rust-analyzer: .ext-rust-analyzer-$(RUST_ANALYZER_VERSION) ; ln -fsT $< $@
.ext-rust-analyzer-$(RUST_ANALYZER_VERSION):
	$(CURL) https://github.com/$(RUST_ANALYZER_GITHUB)/releases/download/$(RUST_ANALYZER_VERSION)/rust-analyzer-x86_64-unknown-linux-gnu.gz \
		| gunzip >$@
	chmod +x $@

all: .x-rustup-init
.x-rustup-init: | .ext-rustup-init ; ln -fsT $| $@
.ext-rustup-init: .ext-rustup-init-$(RUSTUP_VERSION) ; ln -fsT $< $@
.ext-rustup-init-$(RUSTUP_VERSION):
	$(CURL) -o $@ https://static.rust-lang.org/rustup/archive/$(RUSTUP_VERSION)/x86_64-unknown-linux-gnu/rustup-init
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
	$(CURL) -C - -o $@ https://downloads.haskell.org/~hls/haskell-language-server-$(HLS_VERSION)/haskell-language-server-$(HLS_VERSION)-x86_64-linux-deb10.tar.xz

all: tz
tz: | .ext-tz ; ln -fsT $|/$@ $@
.ext-tz: .ext-tz-$(TZ_VERSION) ; ln -fsT $< $@
.ext-tz-$(TZ_VERSION):
	mkdir -p $@
	$(CURL) https://github.com/$(TZ_GITHUB)/releases/download/$(TZ_VERSION)/tz-$(TZ_VERSION)-linux-amd64.tar.gz \
		| tar -x -z -C $@ tz

all: delta
delta: | .ext-delta ; ln -fsT $|/$@ $@
.ext-delta: .ext-delta-$(DELTA_VERSION) ; ln -fsT $< $@
.ext-delta-$(DELTA_VERSION):
	mkdir -p $@
	$(CURL) https://github.com/$(DELTA_GITHUB)/releases/download/$(DELTA_VERSION)/delta-$(DELTA_VERSION)-x86_64-unknown-linux-gnu.tar.gz \
		| tar -x -z -C $@ --strip-components=1 --wildcards "*/delta"

all: pandoc
pandoc: | .ext-pandoc ; ln -fsT $|/bin/$@ $@
.ext-pandoc: .ext-pandoc-$(PANDOC_VERSION) ; ln -fsT $< $@
.ext-pandoc-$(PANDOC_VERSION):
	mkdir -p $@
	$(CURL) https://github.com/$(PANDOC_GITHUB)/releases/download/$(PANDOC_VERSION)/pandoc-$(PANDOC_VERSION)-linux-amd64.tar.gz \
		| tar -x -z -C $@ --strip-components=1

all: hlint
hlint: | .ext-hlint ; ln -fsT $|/$@ $@
.ext-hlint: .ext-hlint-$(HLINT_VERSION) ; ln -fsT $< $@
.ext-hlint-$(HLINT_VERSION):
	mkdir -p $@
	$(CURL) https://github.com/$(HLINT_GITHUB)/releases/download/$(HLINT_VERSION)/hlint-$(HLINT_VERSION:v%=%)-x86_64-linux.tar.gz \
		| tar -x -z -C $@ --strip-components=1

all: .ext-languagetool
.SKIP_GITIGNORE: .ext-languagetool
.ext-languagetool: .ext-languagetool-$(LANGUAGETOOL_VERSION) ; ln -fsT $< $@
.ext-languagetool-$(LANGUAGETOOL_VERSION): .ext-languagetool-$(LANGUAGETOOL_VERSION).zip
	mkdir -p $@
	unzip -d $@ $<
	mv -T $@/LanguageTool-$(LANGUAGETOOL_VERSION:v%=%) $@.tmp
	mv -T $@.tmp $@

.INTERMEDIATE: .ext-languagetool-$(LANGUAGETOOL_VERSION).zip
.ext-languagetool-$(LANGUAGETOOL_VERSION).zip:
	$(CURL) -C - -o $@ https://languagetool.org/download/LanguageTool-$(LANGUAGETOOL_VERSION:v%=%).zip

all: hadolint
hadolint: | .ext-hadolint ; ln -fsT $| $@
.ext-hadolint: .ext-hadolint-$(HADOLINT_VERSION) ; ln -fsT $< $@
.ext-hadolint-$(HADOLINT_VERSION):
	$(CURL) -o $@ https://github.com/$(HADOLINT_GITHUB)/releases/download/$(HADOLINT_VERSION)/hadolint-Linux-x86_64
	chmod +x $@

all: buildifier
buildifier: | .ext-buildifier ; ln -fsT $| $@
.ext-buildifier: .ext-buildifier-$(BUILDIFIER_VERSION) ; ln -fsT $< $@
.ext-buildifier-$(BUILDIFIER_VERSION):
	$(CURL) -o $@ https://github.com/$(BUILDIFIER_GITHUB)/releases/download/$(BUILDIFIER_VERSION)/buildifier-linux-amd64
	chmod +x $@

all: tilt
tilt: | .ext-tilt
	ln -fsT $|/$@ $@
	./$@ analytics opt out
.ext-tilt: .ext-tilt-$(TILT_VERSION) ; ln -fsT $< $@
.ext-tilt-$(TILT_VERSION):
	mkdir -p $@
	$(CURL) https://github.com/$(TILT_GITHUB)/releases/download/$(TILT_VERSION)/tilt.$(TILT_VERSION:v%=%).linux.x86_64.tar.gz \
		| tar -x -z -C $@ tilt
	sudo setcap cap_net_bind_service+ep $@/tilt

all: ctlptl
ctlptl: | .ext-ctlptl
	ln -fsT $|/$@ $@
	./$@ analytics opt out
.ext-ctlptl: .ext-ctlptl-$(CTLPTL_VERSION) ; ln -fsT $< $@
.ext-ctlptl-$(CTLPTL_VERSION):
	mkdir -p $@
	$(CURL) https://github.com/$(CTLPTL_GITHUB)/releases/download/$(CTLPTL_VERSION)/ctlptl.$(CTLPTL_VERSION:v%=%).linux.x86_64.tar.gz \
		| tar -x -z -C $@ ctlptl

all: kind
kind: | .ext-kind ; ln -fsT $| $@
.ext-kind: .ext-kind-$(KIND_VERSION) ; ln -fsT $< $@
.ext-kind-$(KIND_VERSION):
	$(CURL) -o $@ https://github.com/$(KIND_GITHUB)/releases/download/$(KIND_VERSION)/kind-linux-amd64
	chmod +x $@

all: cargo-binstall
cargo-binstall: | .ext-cargo-binstall ; ln -fsT $|/$@ $@
.ext-cargo-binstall: .ext-cargo-binstall-$(CARGO_BINSTALL_VERSION) ; ln -fsT $< $@
.ext-cargo-binstall-$(CARGO_BINSTALL_VERSION):
	mkdir -p $@
	$(CURL) https://github.com/$(CARGO_BINSTALL_GITHUB)/releases/download/$(CARGO_BINSTALL_VERSION)/cargo-binstall-x86_64-unknown-linux-gnu.tgz \
		| tar -x -z -C $@ cargo-binstall

all: cargo-audit
cargo-audit: | .ext-cargo-audit ; ln -fsT $|/$@ $@
.ext-cargo-audit: .ext-cargo-audit-$(CARGO_AUDIT_VERSION) ; ln -fsT $< $@
.ext-cargo-audit-$(CARGO_AUDIT_VERSION):
	mkdir -p $@
	$(CURL) https://github.com/rustsec/rustsec/releases/download/cargo-audit/$(CARGO_AUDIT_VERSION)/cargo-audit-x86_64-unknown-linux-gnu-$(CARGO_AUDIT_VERSION).tgz \
		| tar -x -z -C $@ --strip-components=1 --wildcards "*/cargo-audit"

all: sccache
sccache: | .ext-sccache ; ln -fsT $|/$@ $@
.ext-sccache: .ext-sccache-$(SCCACHE_VERSION) ; ln -fsT $< $@
.ext-sccache-$(SCCACHE_VERSION):
	mkdir -p $@
	$(CURL) https://github.com/$(SCCACHE_GITHUB)/releases/download/$(SCCACHE_VERSION)/sccache-$(SCCACHE_VERSION)-x86_64-unknown-linux-musl.tar.gz \
		| tar -x -z -C $@ --strip-components=1 --wildcards "*/sccache"
	chmod +x $@/sccache

all: fclones
fclones: | .ext-fclones ; ln -fsT $|/$@ $@
.ext-fclones: .ext-fclones-$(FCLONES_VERSION) ; ln -fsT $< $@
.ext-fclones-$(FCLONES_VERSION):
	mkdir -p $@
	$(CURL) https://github.com/$(FCLONES_GITHUB)/releases/download/$(FCLONES_VERSION)/fclones-$(FCLONES_VERSION:v%=%)-linux-glibc-x86_64.tar.gz \
		| tar -x -z -C $@ --strip-components=3 ./usr/bin/fclones

all: docker-buildx
docker-buildx: | .ext-docker-buildx ; ln -fsT $| $@
.ext-docker-buildx: .ext-docker-buildx-$(DOCKER_BUILDX_VERSION) ; ln -fsT $< $@
.ext-docker-buildx-$(DOCKER_BUILDX_VERSION):
	$(CURL) -o $@ https://github.com/$(DOCKER_BUILDX_GITHUB)/releases/download/$(DOCKER_BUILDX_VERSION)/buildx-$(DOCKER_BUILDX_VERSION).linux-amd64
	chmod +x $@

all: docker-compose
docker-compose: | .ext-docker-compose ; ln -fsT $| $@
.ext-docker-compose: .ext-docker-compose-$(DOCKER_COMPOSE_VERSION) ; ln -fsT $< $@
.ext-docker-compose-$(DOCKER_COMPOSE_VERSION):
	$(CURL) -o $@ https://github.com/$(DOCKER_COMPOSE_GITHUB)/releases/download/$(DOCKER_COMPOSE_VERSION)/docker-compose-linux-x86_64
	chmod +x $@

all: .ext-fitsdk
.SKIP_GITIGNORE: .ext-fitsdk
.ext-fitsdk: .ext-fitsdk-$(FITSDK_VERSION) ; ln -fsT $< $@
.ext-fitsdk-$(FITSDK_VERSION): .ext-fitsdk-$(FITSDK_VERSION).zip
	mkdir -p $@
	unzip -d $@ -j $< java/FitCSVTool.jar java/fit.jar

.INTERMEDIATE: .ext-fitsdk-$(FITSDK_VERSION).zip
.ext-fitsdk-$(FITSDK_VERSION).zip:
	$(CURL) -C - -o $@ https://developer.garmin.com/downloads/fit/sdk/FitSDKRelease_$(FITSDK_VERSION).zip

all: dive
dive: | .ext-dive ; ln -fsT $|/$@ $@
.ext-dive: .ext-dive-$(DIVE_VERSION) ; ln -fsT $< $@
.ext-dive-$(DIVE_VERSION):
	mkdir -p $@
	$(CURL) https://github.com/$(DIVE_GITHUB)/releases/download/$(DIVE_VERSION)/dive_$(DIVE_VERSION:v%=%)_linux_amd64.tar.gz \
		| tar -x -z -C $@ dive

all: evcxr
evcxr: | .ext-evcxr ; ln -fsT $|/$@ $@
.ext-evcxr: .ext-evcxr-$(EVCXR_VERSION) ; ln -fsT $< $@
.ext-evcxr-$(EVCXR_VERSION):
	mkdir -p $@
	$(CURL) https://github.com/$(EVCXR_GITHUB)/releases/download/$(EVCXR_VERSION)/evcxr-$(EVCXR_VERSION)-x86_64-unknown-linux-gnu.tar.gz \
		| tar -x -z -C $@ --strip-components=1 --wildcards "*/evcxr"

all: glab
glab: | .ext-glab ; ln -fsT $|/$@ $@
.ext-glab: .ext-glab-$(GLAB_VERSION) ; ln -fsT $< $@
.ext-glab-$(GLAB_VERSION):
	mkdir -p $@
	$(CURL) https://gitlab.com/$(GLAB_GITLAB)/-/releases/$(GLAB_VERSION)/downloads/glab_$(GLAB_VERSION:v%=%)_Linux_x86_64.tar.gz \
		| tar -x -z -C $@ --strip-components=1 bin/glab

.PHONY: gc
## Remove old versions
gc:
	liskin-gc-unlinked $(wildcard .ext-*)

.PHONY: clean
## Remove downloaded stuff
clean:
	$(RM) -r $(wildcard .ext-*)

include ~/_github.mk
include ~/_gitlab.mk
include ~/_gitignore.mk
include ~/_help.mk

.DELETE_ON_ERROR:
