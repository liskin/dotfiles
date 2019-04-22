#!/bin/bash

if xrandr --listproviders | grep -q NVIDIA-0; then
	xrandr --setprovideroutputsource modesetting NVIDIA-0
	xrandr --auto
	export _LISKIN_NVIDIA_ENV=1
fi

. ~/.bash_profile

~/bin/layout-normal
#xkbset sticky -twokey -latchlock && xkbset exp 120 =sticky =twokey =latchlock

#ulimit -c unlimited
while :; do
	xmonad
	sleep 1
done



#~/bin/layout-normal
#xrandr --output LVDS --set BACKLIGHT_CONTROL kernel
#xset r rate 200 25
#bsetroot -mod 5 5 -fg rgb:00/10/00 -bg rgb:00/00/00
#~/.xmonad/startup &
