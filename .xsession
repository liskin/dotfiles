#!/usr/bin/env bash

shopt -s nullglob

# prevent launching pkttyagent and being stopped by SIGTTIN
exec </dev/null

# ssh-agent is setgid and drops LD_* vars, reload them
. ~/.bashrc.d/10_env.sh

for xres in $(run-parts --list /etc/X11/Xresources); do xrdb -merge "$xres"; done
for xres in ~/.Xresources.d/*.{ad,m4}; do xrdb -cpp m4 -merge "$xres"; done

if xrandr --listproviders | grep -q NVIDIA-0; then
	xrandr --setprovideroutputsource modesetting NVIDIA-0
	xrandr --auto
fi

layout-normal

xset r rate 200 25
xset b off
xset s 480 5
xset dpms 600 600 600
xsetroot -cursor_name left_ptr
setxkbmap ${_LISKIN_NVIDIA:+-option terminate:ctrl_alt_bksp}
xmodmap ~/.Xmodmap
xinput set-prop 'SynPS/2 Synaptics TouchPad' 'libinput Natural Scrolling Enabled' 1 || :
xinput set-prop 'SynPS/2 Synaptics TouchPad' 'libinput Accel Speed' 0.3 || :
xinput set-prop 'SynPS/2 Synaptics TouchPad' 'libinput Middle Emulation Enabled' 1 || :
xinput set-prop 'SynPS/2 Synaptics TouchPad' 'libinput Disable While Typing Enabled' 0 || :
xinput set-prop 'TPPS/2 IBM TrackPoint' 'libinput Accel Speed' -0.3 || :

start-pulseaudio-x11
liskin-thinkpad-battery update-xmobar

{
	echo DISPLAY="$DISPLAY"
	echo XDG_SEAT="$XDG_SEAT"
	echo XDG_SESSION_ID="$XDG_SESSION_ID"
	echo XDG_VTNR="$XDG_VTNR"
} >"$XDG_RUNTIME_DIR"/xsession-env-"$DISPLAY"

if [[ "$(loginctl show-user --property=Display "$USER")" == "Display=$XDG_SESSION_ID" ]]; then
	systemctl --user import-environment SSH_AUTH_SOCK
	systemd-run \
		--quiet \
		--collect --wait \
		--user --slice="session.slice" --unit="xmonad-$XDG_SESSION_ID-$DISPLAY.service" \
		--property="EnvironmentFile=$XDG_RUNTIME_DIR/xsession-env-$DISPLAY" \
		--property="Requires=graphical-session-pre.target" \
		--property="After=graphical-session-pre.target" \
		--property="BindsTo=graphical-session.target" \
		--property="BindsTo=xmonad@$DISPLAY.target" \
		--property="OnFailure=failsafe-xsecurelock@$DISPLAY.service" \
		--property="Restart=on-failure" \
		-- xmonad
else
	# workaround for https://gitlab.freedesktop.org/pulseaudio/pulseaudio/-/issues/1076
	# (also, we don't want AudioPlay/Pause keys in more than one X server)
	xprop -root -remove PULSE_ID
	stop-pulseaudio-x11

	systemd-run \
		--quiet \
		--collect --scope \
		--user --slice="session.slice" --unit="xmonad-$XDG_SESSION_ID-$DISPLAY.scope" \
		--property="BindsTo=xmonad@$DISPLAY.target" \
		-- xmonad
fi

systemctl --user start "failsafe-xsecurelock@$DISPLAY.service"
stop-pulseaudio-x11
sleep 2
