#!/usr/bin/env bash

shopt -s nullglob

# ssh-agent is setgid and drops LD_* vars, reload them
. ~/.bashrc.d/10_env.sh

for xres in $(run-parts --list /etc/X11/Xresources); do xrdb -merge "$xres"; done
for xres in ~/.Xresources.d/*.{ad,m4}; do xrdb -cpp m4 -merge "$xres"; done

if xrandr --listproviders | grep -q NVIDIA-0; then
	xrandr --setprovideroutputsource modesetting NVIDIA-0
	xrandr --auto
fi

~/bin/layout-normal

xset r rate 200 25
xset b off
xset s 480 5
xset dpms 600 600 600
xsetroot -cursor_name left_ptr
setxkbmap
xmodmap ~/.Xmodmap
xinput set-prop 'SynPS/2 Synaptics TouchPad' 'libinput Natural Scrolling Enabled' 1 || :
xinput set-prop 'SynPS/2 Synaptics TouchPad' 'libinput Accel Speed' 0.3 || :
xinput set-prop 'SynPS/2 Synaptics TouchPad' 'libinput Middle Emulation Enabled' 1 || :
xinput set-prop 'SynPS/2 Synaptics TouchPad' 'libinput Disable While Typing Enabled' 0 || :
xinput set-prop 'TPPS/2 IBM TrackPoint' 'libinput Accel Speed' -0.3 || :

function alttab {
	# wait for xmonad to start
	until wmctrl -m &>/dev/null && wmctrl -l &>/dev/null; do
		sleep 0.5
	done

	exec alttab
}

start-pulseaudio-x11
liskin-xss-lock &
alttab &
liskin-thinkpad-battery update-xmobar

if [[ "$(loginctl show-user --property=Display "$USER")" == "Display=$XDG_SESSION_ID" ]]; then
	systemctl --user --no-block start \
		redshift.service \
		notify-osd.service \
		battery-watch.service \
		blueman-applet.service \
		arbtt-capture.service
	pasystray --notify=all &
	nm-applet &
	/usr/lib/policykit-1-gnome/polkit-gnome-authentication-agent-1 &
	firewall-applet &
	caffeine-indicator &

	systemctl --user import-environment SSH_AUTH_SOCK
	exec systemd-run \
		--quiet \
		--wait --collect \
		--user --slice="session.slice" --unit="xmonad@$XDG_SESSION_ID-$DISPLAY.service" \
		${XDG_SEAT:+--setenv="XDG_SEAT=$XDG_SEAT"} \
		${XDG_SESSION_ID:+--setenv="XDG_SESSION_ID=$XDG_SESSION_ID"} \
		${XDG_VTNR:+--setenv="XDG_VTNR=$XDG_VTNR"} \
		--property="Requires=graphical-session-pre.target" \
		--property="After=graphical-session-pre.target" \
		--property="BindsTo=graphical-session.target" \
		--property="Restart=on-failure" \
		-- ~/bin/xmonad
else
	exec ~/bin/xmonad
fi
