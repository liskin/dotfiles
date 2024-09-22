.PHONY: all
all:

CURL := curl -LSsf

BUILDIFIER_VERSION := v7.1.2
CIRCLECI_CLI_VERSION := v0.1.30549
DOCKER_BUILDX_VERSION := v0.16.2
DOCKER_COMPOSE_VERSION := v2.29.2
FITSDK_VERSION := 21.84.00
GLAB_VERSION := v1.45.0
HLINT_VERSION := v3.8
HLS_GHC_VERSION := 9.4.7
HLS_VERSION := 2.4.0.0
JIRA_CLI_VERSION := v1.5.1
LANGUAGETOOL_VERSION := v6.4
LUALS_VERSION := 3.10.5
NVIM_VERSION := v0.10.1
PANDOC_VERSION := 3.2
UBI_VERSION := v0.1.1

BUILDIFIER_GITHUB := bazelbuild/buildtools
CIRCLECI_CLI_GITHUB := CircleCI-Public/circleci-cli
DOCKER_BUILDX_GITHUB := docker/buildx
DOCKER_COMPOSE_GITHUB := docker/compose
GLAB_GITLAB := gitlab-org/cli
HLINT_GITHUB := ndmitchell/hlint
HLS_GITHUB := haskell/haskell-language-server
JIRA_CLI_GITHUB := ankitpokhrel/jira-cli
LANGUAGETOOL_GITHUB_LASTTAG := languagetool-org/languagetool
LUALS_GITHUB := LuaLS/lua-language-server
NVIM_GITHUB_LASTTAG := neovim/neovim
PANDOC_GITHUB := jgm/pandoc
UBI_GITHUB := houseabsolute/ubi

.PHONY: update
## Check for updates
update: update-github
update: update-gitlab

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

all: buildifier
buildifier: | .ext-buildifier ; ln -fsT $| $@
.ext-buildifier: .ext-buildifier-$(BUILDIFIER_VERSION) ; ln -fsT $< $@
.ext-buildifier-$(BUILDIFIER_VERSION):
	$(CURL) -o $@ https://github.com/$(BUILDIFIER_GITHUB)/releases/download/$(BUILDIFIER_VERSION)/buildifier-linux-amd64
	chmod +x $@

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

all: glab
glab: | .ext-glab ; ln -fsT $|/$@ $@
.ext-glab: .ext-glab-$(GLAB_VERSION) ; ln -fsT $< $@
.ext-glab-$(GLAB_VERSION):
	mkdir -p $@
	$(CURL) https://gitlab.com/$(GLAB_GITLAB)/-/releases/$(GLAB_VERSION)/downloads/glab_$(GLAB_VERSION:v%=%)_Linux_x86_64.tar.gz \
		| tar -x -z -C $@ --strip-components=1 bin/glab

all: .ext-circleci-cli
.SKIP_GITIGNORE: .ext-circleci-cli
.ext-circleci-cli: .ext-circleci-cli-$(CIRCLECI_CLI_VERSION) ; ln -fsT $< $@
.ext-circleci-cli-$(CIRCLECI_CLI_VERSION): .ext-circleci-cli-$(CIRCLECI_CLI_VERSION).tar.gz
	mkdir -p $@
	tar -xzf $< -C $@ --strip-components=1 --wildcards "*/circleci"

.INTERMEDIATE: .ext-circleci-cli-$(CIRCLECI_CLI_VERSION).tar.gz
.ext-circleci-cli-$(CIRCLECI_CLI_VERSION).tar.gz:
	wget -O $@ https://github.com/$(CIRCLECI_CLI_GITHUB)/releases/download/$(CIRCLECI_CLI_VERSION)/circleci-cli_$(CIRCLECI_CLI_VERSION:v%=%)_linux_amd64.tar.gz

all: nvim
nvim: | .ext-nvim ; ln -fsT $|/bin/$@ $@
.ext-nvim: .ext-nvim-$(NVIM_VERSION) ; ln -fsT $< $@
.ext-nvim-$(NVIM_VERSION): .ext-nvim-$(NVIM_VERSION).tar.gz
	mkdir -p $@
	tar -xzf $< -C $@ --strip-components=1

.INTERMEDIATE: .ext-nvim-$(NVIM_VERSION).tar.gz
.ext-nvim-$(NVIM_VERSION).tar.gz:
	wget -O $@ https://github.com/$(NVIM_GITHUB_LASTTAG)/releases/download/$(NVIM_VERSION)/nvim-linux64.tar.gz

all: lua-language-server
lua-language-server: | .ext-luals ; ln -fsT $|/bin/$@ $@
.ext-luals: .ext-luals-$(LUALS_VERSION) ; ln -fsT $< $@
.ext-luals-$(LUALS_VERSION): .ext-luals-$(LUALS_VERSION).tar.gz
	mkdir -p $@
	tar -xzf $< -C $@

.INTERMEDIATE: .ext-luals-$(LUALS_VERSION).tar.gz
.ext-luals-$(LUALS_VERSION).tar.gz:
	wget -O $@ https://github.com/$(LUALS_GITHUB)/releases/download/$(LUALS_VERSION)/lua-language-server-$(LUALS_VERSION)-linux-x64.tar.gz

all: jira
jira: | .ext-jira ; ln -fsT $|/$@ $@
.ext-jira: .ext-jira-$(JIRA_CLI_VERSION) ; ln -fsT $< $@
.ext-jira-$(JIRA_CLI_VERSION):
	mkdir -p $@
	$(CURL) https://github.com/$(JIRA_CLI_GITHUB)/releases/download/$(JIRA_CLI_VERSION)/jira_$(JIRA_CLI_VERSION:v%=%)_linux_x86_64.tar.gz \
		| tar -x -z -C $@ --strip-components=2 --wildcards "*/bin/jira"

# TODO: drop when https://github.com/jdx/mise/pull/2290 merged
all: ubi
ubi: | .ext-ubi ; ln -fsT $|/$@ $@
.ext-ubi: .ext-ubi-$(UBI_VERSION) ; ln -fsT $< $@
.ext-ubi-$(UBI_VERSION):
	mkdir -p $@
	$(CURL) https://github.com/$(UBI_GITHUB)/releases/download/$(UBI_VERSION)/ubi-Linux-x86_64-musl.tar.gz \
		| tar -x -z -C $@ ubi

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
