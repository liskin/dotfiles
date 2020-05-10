#!/usr/bin/env bash

# ssh-agent is setgid and drops LD_* vars, reload them
. ~/.bashrc.d/10_env.sh

if xrandr --listproviders | grep -q NVIDIA-0; then
	xrandr --setprovideroutputsource modesetting NVIDIA-0
	xrandr --auto
	export _LISKIN_NVIDIA_ENV=1
fi

~/bin/layout-normal

function alttab {
	# wait for xmonad to start
	until wmctrl -m &>/dev/null && wmctrl -l &>/dev/null; do
		sleep 0.5
	done

	exec alttab
}

xset r rate 200 25
xset b off
xset s 480 5
xset dpms 600 600 600
xsetroot -cursor_name left_ptr
setxkbmap
xmodmap ~/.Xmodmap
start-pulseaudio-x11
liskin-xss-lock &
alttab &

liskin-thinkpad-battery update-xmobar

if [[ $DISPLAY == :0 ]]; then
	pasystray --notify=all &
	nm-applet &
	blueman-applet &
	/usr/lib/policykit-1-gnome/polkit-gnome-authentication-agent-1 &
	firewall-applet &
	caffeine-indicator &
fi

while :; do
	xmonad
	sleep 1
done
