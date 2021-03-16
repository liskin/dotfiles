.PHONY: all
all: .Inconsolata.zip.unzip

.INTERMEDIATE: nerd-fonts-releases.json
nerd-fonts-releases.json:
	gh api repos/ryanoasis/nerd-fonts/releases/latest > $@

%.zip: nerd-fonts-releases.json
	curl -LSsf -o $@ "$$(jq -r '.assets[] | select(.name == "$@") | .browser_download_url' $<)"

.%.unzip: %
	unzip $< -x "*Windows Compatible*" "*.otf"
	touch $@
