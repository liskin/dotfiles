.PHONY: all
all:

HOSTNAME := $(shell hostname)

REDOC_CLI_VERSION := 0.13.13
REDOCLY_VERSION := 1.0.0-beta.97

all: $(if $(filter pftp.%,$(HOSTNAME)),redoc-cli)
redoc-cli: | .ext-redoc-cli ; ln -fsT $|/bin/$@ $@
.ext-redoc-cli: .ext-redoc-cli-$(REDOC_CLI_VERSION) ; ln -fsT $< $@
.ext-redoc-cli-$(REDOC_CLI_VERSION):
	bwrap-npm-install --prefix $@ -- --omit=dev redoc-cli@$(REDOC_CLI_VERSION)

all: $(if $(filter pftp.%,$(HOSTNAME)),redocly)
redocly: | .ext-redocly ; ln -fsT $|/bin/$@ $@
.ext-redocly: .ext-redocly-$(REDOCLY_VERSION) ; ln -fsT $< $@
.ext-redocly-$(REDOCLY_VERSION):
	bwrap-npm-install --prefix $@ -- --omit=dev @redocly/cli@$(REDOCLY_VERSION)

.PHONY: gc
gc:
	liskin-gc-unlinked $(wildcard .ext-*)
	fclones group --no-ignore --hidden . | fclones link

.PHONY: clean
clean:
	$(RM) -r $(wildcard .ext-*)

include ~/_gitignore.mk
