#!bash

if [[ ! $DISPLAY && $XDG_SESSION_TYPE == "x11" ]]; then
	[[ "$(loginctl show-user --property=Display "$USER")" == "Display=$XDG_SESSION_ID" ]] && primary=: || primary=

	. ~/.bashrc.d/10_env.sh

	[[ $_LISKIN_NVIDIA ]] && startx=(startx-nvidia) || startx=(startx)
	journal=(/usr/bin/systemd-cat --priority=info --stderr-priority=warning --level-prefix=false)
	[[ $primary ]] && session=(/etc/X11/xinit/xinitrc) || session=(~/.xsession)

	exec "${startx[@]}" "${journal[@]}" "${session[@]}"
	exit 1
fi

. ~/.bashrc
