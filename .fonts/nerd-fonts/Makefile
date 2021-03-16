.PHONY: all
all: .DejaVuSansMono.zip.unzip NotoEmojiNerdFontComplete.ttf

.INTERMEDIATE: nerd-fonts-releases.json
nerd-fonts-releases.json:
	gh api repos/ryanoasis/nerd-fonts/releases/latest > $@

%.zip: nerd-fonts-releases.json
	curl -LSsf -o $@ "$$(jq -r '.assets[] | select(.name == "$@") | .browser_download_url' $<)"

.%.unzip: %
	unzip $< -x "*Windows Compatible*"
	touch $@

NotoEmojiNerdFontComplete.ttf:
	curl -LSsf -o $@ https://github.com/ryanoasis/nerd-fonts/raw/master/patched-fonts/Noto/Emoji/complete/Noto%20Emoji%20Nerd%20Font%20Complete.ttf
