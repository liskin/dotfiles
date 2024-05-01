.PHONY: all
all: .Inconsolata.zip.unzip .Gohu.zip.unzip

.INTERMEDIATE: nerd-fonts-releases-v2.2.2.json
.INTERMEDIATE: nerd-fonts-releases-v3.2.1.json
nerd-fonts-releases-%.json:
	gh api repos/ryanoasis/nerd-fonts/releases/tags/$* > $@

.INTERMEDIATE: Inconsolata.zip
Inconsolata.zip: nerd-fonts-releases-v2.2.2.json

.INTERMEDIATE: Gohu.zip
Gohu.zip: nerd-fonts-releases-v3.2.1.json

%.zip:
	curl -LSsf -o $@ "$$(jq -r '.assets[] | select(.name == "$@") | .browser_download_url' $<)"

.%.unzip: %
	unzip $< "*.[to]tf" -x "*Windows Compatible*"
	touch $@

.DELETE_ON_ERROR:
