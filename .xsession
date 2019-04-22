#!/bin/bash

. ~/.bash_profile

~/bin/layout-normal

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
