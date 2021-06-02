stack.yaml: ~/src-haskell/stack.yaml Makefile
	yq --yaml-output '{resolver: .resolver, packages: []}' $< >$@

.DELETE_ON_ERROR:
