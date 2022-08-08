.PHONY: all
all:

HOSTNAME := $(shell hostname)

REDOCLY_VERSION := 1.0.0-beta.105
REDOC_CLI_VERSION := 0.13.17

REDOCLY_NPM := @redocly/cli
REDOC_CLI_NPM := redoc-cli

define NPM_CHECK_LATEST_RELEASE
	{ \
		v="$$(npm view $($(1)) version)"; \
		if [ "$$v" != "$($(2))" ]; then echo "$(2) := $$v"; fi; \
	};
endef

.PHONY: update
## Check for updates
update:
	@$(foreach repo_var,$(filter %_NPM,$(.VARIABLES)),\
		$(call NPM_CHECK_LATEST_RELEASE,$(repo_var),$(repo_var:_NPM=_VERSION)))

all: $(if $(filter pftp.%,$(HOSTNAME)),redoc-cli)
redoc-cli: | .ext-redoc-cli ; ln -fsT $|/bin/$@ $@
.ext-redoc-cli: .ext-redoc-cli-$(REDOC_CLI_VERSION) ; ln -fsT $< $@
.ext-redoc-cli-$(REDOC_CLI_VERSION):
	bwrap-npm-install --prefix $@ -- --omit=dev $(REDOC_CLI_NPM)@$(REDOC_CLI_VERSION)

all: $(if $(filter pftp.%,$(HOSTNAME)),redocly)
redocly: | .ext-redocly ; ln -fsT $|/bin/$@ $@
.ext-redocly: .ext-redocly-$(REDOCLY_VERSION) ; ln -fsT $< $@
.ext-redocly-$(REDOCLY_VERSION):
	bwrap-npm-install --prefix $@ -- --omit=dev $(REDOCLY_NPM)@$(REDOCLY_VERSION)

.PHONY: gc
gc:
	liskin-gc-unlinked $(wildcard .ext-*)
	fclones group --no-ignore --hidden . | fclones link

.PHONY: clean
clean:
	$(RM) -r $(wildcard .ext-*)

include ~/_gitignore.mk
