.PHONY: all
all: Fixed.otb FixedBold.otb FixedOblique.otb

MISC=/usr/share/fonts/X11/misc

Fixed.otb: $(addprefix $(MISC)/,$(shell awk '/misc-fixed-medium-r-normal--.*-iso10646-1/ { print $$1 }' $(MISC)/fonts.dir))
	fonttosfnt -r -o $@ $^

FixedBold.otb: $(addprefix $(MISC)/,$(shell awk '/misc-fixed-bold-r-normal--.*-iso10646-1/ { print $$1 }' $(MISC)/fonts.dir))
	fonttosfnt -r -o $@ $^

FixedOblique.otb: $(addprefix $(MISC)/,$(shell awk '/misc-fixed-medium-o-normal--.*-iso10646-1/ { print $$1 }' $(MISC)/fonts.dir))
	fonttosfnt -r -o $@ $^

include ~/.gitignore.mk
