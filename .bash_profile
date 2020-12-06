#!bash

if [[ ! $DISPLAY && $XDG_SESSION_TYPE == "x11" ]]; then
	. ~/.bashrc.d/10_env.sh

	[[ "$(loginctl show-user --property=Display "$USER")" == "Display=$XDG_SESSION_ID" ]] && primary=: || primary=
	[[ $_LISKIN_NVIDIA ]] && startx="startx-nvidia" || startx=startx

	exec $startx ${primary:+/etc/X11/xinit/xinitrc}
	exit 1
fi

. ~/.bashrc
