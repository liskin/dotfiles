.PHONY: all
##
all:

.PHONY: $(filter attributes,$(MAKECMDGOALS))
all: attributes
.SKIP_GITIGNORE: attributes
##
attributes: ~/bin/liskin-include-preproc.py attributes.d/mergiraf
	liskin-include-preproc.py $@

.PHONY: $(if $(filter attributes,$(MAKECMDGOALS)),attributes.d/mergiraf)
attributes.d/mergiraf:
	mergiraf languages --gitattributes > $@

include ~/_gitignore.mk
include ~/_help.mk
