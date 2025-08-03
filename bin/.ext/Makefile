.PHONY: all
all:

HOSTNAME := $(shell hostname)

ARCH := $(shell uname -m)
ARM64 := $(if $(filter aarch64,$(ARCH)),1)

CURL := curl -LSsf

DOCKER_BUILDX_VERSION := v0.26.1
DOCKER_COMPOSE_VERSION := v2.39.1
FITSDK_VERSION := 21.84.00
HLINT_VERSION := v3.8
HLS_GHC_VERSION := 9.6.6
HLS_VERSION := 2.9.0.1
LANGUAGETOOL_VERSION := v6.4
PANDOC_VERSION := 3.5
DOTNET8_VERSION := 8.0.410
DOTNET9_VERSION := 9.0.301

DOCKER_BUILDX_GITHUB := docker/buildx
DOCKER_COMPOSE_GITHUB := docker/compose
HLINT_GITHUB := ndmitchell/hlint
HLS_GITHUB := haskell/haskell-language-server
LANGUAGETOOL_GITHUB_LASTTAG := languagetool-org/languagetool
PANDOC_GITHUB := jgm/pandoc

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
	$(CURL) -C - -o $@ https://downloads.haskell.org/~hls/haskell-language-server-$(HLS_VERSION)/haskell-language-server-$(HLS_VERSION)-$(if $(ARM64),aarch64-linux-ubuntu20,x86_64-linux-deb11).tar.xz

all: pandoc
pandoc: | .ext-pandoc ; ln -fsT $|/bin/$@ $@
.ext-pandoc: .ext-pandoc-$(PANDOC_VERSION) ; ln -fsT $< $@
.ext-pandoc-$(PANDOC_VERSION):
	mkdir -p $@
	$(CURL) https://github.com/$(PANDOC_GITHUB)/releases/download/$(PANDOC_VERSION)/pandoc-$(PANDOC_VERSION)-linux-$(if $(ARM64),arm64,amd64).tar.gz \
		| tar -x -z -C $@ --strip-components=1

ifeq "$(ARM64)" ""
all: hlint
hlint: | .ext-hlint ; ln -fsT $|/$@ $@
.ext-hlint: .ext-hlint-$(HLINT_VERSION) ; ln -fsT $< $@
.ext-hlint-$(HLINT_VERSION):
	mkdir -p $@
	$(CURL) https://github.com/$(HLINT_GITHUB)/releases/download/$(HLINT_VERSION)/hlint-$(HLINT_VERSION:v%=%)-x86_64-linux.tar.gz \
		| tar -x -z -C $@ --strip-components=1
endif

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

all: docker-buildx
docker-buildx: | .ext-docker-buildx ; ln -fsT $| $@
.ext-docker-buildx: .ext-docker-buildx-$(DOCKER_BUILDX_VERSION) ; ln -fsT $< $@
.ext-docker-buildx-$(DOCKER_BUILDX_VERSION):
	$(CURL) -o $@ https://github.com/$(DOCKER_BUILDX_GITHUB)/releases/download/$(DOCKER_BUILDX_VERSION)/buildx-$(DOCKER_BUILDX_VERSION).linux-$(if $(ARM64),arm64,amd64)
	chmod +x $@

all: docker-compose
docker-compose: | .ext-docker-compose ; ln -fsT $| $@
.ext-docker-compose: .ext-docker-compose-$(DOCKER_COMPOSE_VERSION) ; ln -fsT $< $@
.ext-docker-compose-$(DOCKER_COMPOSE_VERSION):
	$(CURL) -o $@ https://github.com/$(DOCKER_COMPOSE_GITHUB)/releases/download/$(DOCKER_COMPOSE_VERSION)/docker-compose-linux-$(if $(ARM64),aarch64,x86_64)
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

all: $(if $(filter pf%,$(HOSTNAME)),dotnet)
dotnet: | .ext-dotnet ; ln -fsT $|/$@ $@
.ext-dotnet: .ext-dotnet-$(DOTNET8_VERSION)-$(DOTNET9_VERSION) ; ln -fsT $< $@
.ext-dotnet-$(DOTNET8_VERSION)-$(DOTNET9_VERSION): .ext-dotnet-$(DOTNET8_VERSION).tar.gz .ext-dotnet-$(DOTNET9_VERSION).tar.gz
	mkdir -p $@
	tar -xzf .ext-dotnet-$(DOTNET8_VERSION).tar.gz -C $@
	tar -xzf .ext-dotnet-$(DOTNET9_VERSION).tar.gz -C $@
	touch $@

.INTERMEDIATE: .ext-dotnet-$(DOTNET8_VERSION).tar.gz .ext-dotnet-$(DOTNET9_VERSION).tar.gz
.ext-dotnet-$(DOTNET8_VERSION).tar.gz:
	$(CURL) -C - -o $@ https://builds.dotnet.microsoft.com/dotnet/Sdk/$(DOTNET8_VERSION)/dotnet-sdk-$(DOTNET8_VERSION)-linux-$(if $(ARM64),arm64,x64).tar.gz
.ext-dotnet-$(DOTNET9_VERSION).tar.gz:
	$(CURL) -C - -o $@ https://builds.dotnet.microsoft.com/dotnet/Sdk/$(DOTNET9_VERSION)/dotnet-sdk-$(DOTNET9_VERSION)-linux-$(if $(ARM64),arm64,x64).tar.gz

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
