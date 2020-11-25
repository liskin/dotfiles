#!/usr/bin/env bash

set -eu

if [[ ${__neomutt-} ]]; then
	exit
elif [[ ${TMUX-} || ${STY-} ]]; then
	cat <<-END
	set background_edit=yes
	set editor=~/.config/mutt/bgedit-screen-tmux.sh
	END
fi
