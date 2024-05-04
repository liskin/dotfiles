.PHONY: all
all: .iosevka.zip.unzip

IOSEVKA_VERSION = 30.0.0

.INTERMEDIATE: .iosevka.zip
.iosevka.zip:
	curl -LSsf -o $@ https://github.com/be5invis/Iosevka/releases/download/v$(IOSEVKA_VERSION)/SuperTTC-SGr-IosevkaFixed-$(IOSEVKA_VERSION).zip

.%.unzip: .%
	unzip $< "*.ttc"
	touch $@

.DELETE_ON_ERROR:
