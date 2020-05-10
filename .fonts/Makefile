.PHONY: all
all: Fixed.otb FixedBold.otb FixedOblique.otb

MISC=/usr/share/fonts/X11/misc

Fixed.otb:
	cd $(MISC) && fonttosfnt -r -o $(PWD)/$@ $(shell awk '/misc-fixed-medium-r-normal--.*-iso10646-1/ { print $$1 }' $(MISC)/fonts.dir)

FixedBold.otb:
	cd $(MISC) && fonttosfnt -r -o $(PWD)/$@ $(shell awk '/misc-fixed-bold-r-normal--.*-iso10646-1/ { print $$1 }' $(MISC)/fonts.dir)

FixedOblique.otb:
	cd $(MISC) && fonttosfnt -r -o $(PWD)/$@ $(shell awk '/misc-fixed-medium-o-normal--.*-iso10646-1/ { print $$1 }' $(MISC)/fonts.dir)
