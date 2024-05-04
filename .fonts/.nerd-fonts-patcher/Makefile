.PHONY: all
all: Fixed7x13NFSymbolsMono-Regular.ttf

.INTERMEDIATE: nerd-fonts-releases-v3.2.1.json
nerd-fonts-releases-%.json:
	gh api repos/ryanoasis/nerd-fonts/releases/tags/$* > $@

.INTERMEDIATE: FontPatcher.zip
FontPatcher.zip: nerd-fonts-releases-v3.2.1.json

%.zip:
	curl -LSsf -o $@ "$$(jq -r '.assets[] | select(.name == "$@") | .browser_download_url' $<)"

.%.zip.unzip: %.zip
	unzip -d $* $<
	touch $@

MISC=/usr/share/fonts/X11/misc

.INTERMEDIATE: Fixed7x13.otb
Fixed7x13.otb: $(addprefix $(MISC)/,$(shell awk '/misc-fixed-medium-r-normal--13-.*-70-iso10646-1/ { print $$1 }' $(MISC)/fonts.dir))
	fonttosfnt -r -o $@ $^

.INTERMEDIATE: Fixed7x13.min.ttf
Fixed7x13.min.ttf: Fixed7x13.otb
	fontforge -c 'f = open(argv[1]); f.selection.select("A", "I", "M", "W", "a", "i", "m", ".", "0", "1", "8", "d", "x", "y", "g", "W", "X", "\"", "_", 0x00cd, 0x00d5, 0x0114, 0x0177, 0x0643, 0x304c, 0x672c, ".notdef"); f.selection.invert(); f.clear(); f.generate(argv[2], bitmap_type="otb")' $< $@

Fixed7x13NFSymbolsMono-Regular.ttf: Fixed7x13.min.ttf .FontPatcher.zip.unzip
	fontforge --script FontPatcher/font-patcher --complete --mono --name 'Fixed7x13 NF Symbols Mono' $<

.DELETE_ON_ERROR:
