#!bash

if [[ ! $DISPLAY && $XDG_SESSION_TYPE == "x11" ]]; then
	[[ "$(loginctl show-user --property=Display "$USER")" == "Display=$XDG_SESSION_ID" ]] && primary=: || primary=
	[[ $_LISKIN_NVIDIA ]] && startx="startx-nvidia" || startx="startx"

	. ~/.bashrc.d/10_env.sh
	exec "$startx" ${primary:+/etc/X11/xinit/xinitrc} ~/.xsession-wrapper
	exit 1
fi

. ~/.bashrc
