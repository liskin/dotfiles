.PHONY: all
all: $(patsubst %.control,.build/%_1.0_all.deb,$(wildcard *.control))

.build:
	mkdir $@

.build/%_1.0_all.deb: %.control | .build
	cd .build && equivs-build ../$<
