.PHONY: help
## Display this help
help: COLUMNS=$(shell tput cols)
help:
	@$(MAKE) --silent help-src \
		| perl -Mfeature=say -MText::Wrap -0777 -ne 'while(/((?:^##(?:| .*)$$(?#)\n)+)(^.*:)/gm) { my ($$t, $$h) = ($$2, $$1); $$h =~ s/^## ?//gm; $$h =~ s/\s+/ /g; $$h =~ s/^\s+|\s+$$//g; say "$$t $$h"; }' | fmt $(if $(COLUMNS),-$(COLUMNS)) -t

.PHONY: help-src
help-src: help-src-makefiles

.PHONY: help-src-makefiles
help-src-makefiles:
	@cat $(MAKEFILE_LIST)
