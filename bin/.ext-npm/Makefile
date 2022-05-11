.PHONY: all
all:

REDOC_CLI_VERSION := 0.13.12

all: redoc-cli
redoc-cli: | .ext-redoc-cli ; ln -fsT $|/bin/$@ $@
.ext-redoc-cli: .ext-redoc-cli-$(REDOC_CLI_VERSION) ; ln -fsT $< $@
.ext-redoc-cli-$(REDOC_CLI_VERSION):
	bwrap-npm-install --prefix $@ -- --omit=dev redoc-cli@$(REDOC_CLI_VERSION)

.PHONY: gc
gc:
	liskin-gc-unlinked $(wildcard .ext-*)

.PHONY: clean
clean:
	$(RM) -r $(wildcard .ext-*)

include ~/_gitignore.mk
