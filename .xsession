#!/usr/bin/env bash

if xrandr --listproviders | grep -q NVIDIA-0; then
	xrandr --setprovideroutputsource modesetting NVIDIA-0
	xrandr --auto
	export _LISKIN_NVIDIA_ENV=1
fi

. ~/.bash_profile

~/bin/layout-normal

while :; do
	xmonad
	sleep 1
done
