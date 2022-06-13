.PHONY: all
all: .iosevka.zip.unzip

IOSEVKA_VERSION = 15.5.0

.INTERMEDIATE: .iosevka.zip
.iosevka.zip:
	curl -LSsf -o $@ https://github.com/be5invis/Iosevka/releases/download/v$(IOSEVKA_VERSION)/super-ttc-sgr-iosevka-fixed-ss11-$(IOSEVKA_VERSION).zip

.%.unzip: .%
	unzip $< "*.ttc"
	touch $@

.DELETE_ON_ERROR:
