#!/usr/bin/env bash

shopt -s nullglob

# prevent launching pkttyagent and being stopped by SIGTTIN
exec </dev/null

# ssh-agent is setgid and drops LD_* vars, reload them
. ~/.xsessionrc

for xres in $(run-parts --list /etc/X11/Xresources); do xrdb -merge "$xres"; done
for xres in ~/.Xresources.d/*.{ad,m4}; do xrdb -cpp m4 -merge "$xres"; done

if xrandr --listproviders | grep -q NVIDIA-0; then
	xrandr --setprovideroutputsource modesetting NVIDIA-0
	xrandr --auto
fi

layout-normal
~/bin/.xlayout/post.sh

xset r rate 200 25
xset b off
xset s 480 5
xset dpms 600 600 600
xsetroot -cursor_name left_ptr
setxkbmap ${_LISKIN_NVIDIA:+-option terminate:ctrl_alt_bksp}
xmodmap ~/.Xmodmap
if liskin-dmi-product 20VX; then xmodmap ~/.Xmodmap-thinkpad-6row; fi
xinput set-prop 'SynPS/2 Synaptics TouchPad' 'libinput Natural Scrolling Enabled' 1 || :
xinput set-prop 'SynPS/2 Synaptics TouchPad' 'libinput Middle Emulation Enabled' 1 || :
xinput set-prop 'SynPS/2 Synaptics TouchPad' 'libinput Disable While Typing Enabled' 0 || :
if liskin-dmi-product 20VX; then
	xinput set-prop 'SynPS/2 Synaptics TouchPad' 'libinput Accel Speed' 0.6 || :
	xinput set-prop 'TPPS/2 Elan TrackPoint' 'libinput Accel Speed' -0.5 || :
elif liskin-dmi-product 20K7; then
	xinput set-prop 'SynPS/2 Synaptics TouchPad' 'libinput Accel Speed' 0.3 || :
	xinput set-prop 'TPPS/2 IBM TrackPoint' 'libinput Accel Speed' 0.6 || : # /etc/X11/…
fi

liskin-thinkpad-battery update-xmobar
xprop -root -f _XMOBAR_DND 8u -set _XMOBAR_DND ""

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
		--property="Environment=_LISKIN_SESSION_PRIMARY=1" \
		--property="Requires=graphical-session-pre.target" \
		--property="After=graphical-session-pre.target" \
		--property="BindsTo=graphical-session.target" \
		--property="BindsTo=xmonad@$DISPLAY.target" \
		--property="OnFailure=failsafe-xsecurelock@$DISPLAY.service" \
		--property="Restart=on-failure" \
		-- xmonad
else
	systemd-run \
		--quiet \
		--collect --scope \
		--user --slice="session.slice" --unit="xmonad-$XDG_SESSION_ID-$DISPLAY.scope" \
		--property="BindsTo=xmonad@$DISPLAY.target" \
		-- xmonad
fi

systemctl --user start "failsafe-xsecurelock@$DISPLAY.service"
sleep 2
