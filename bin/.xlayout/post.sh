#!/usr/bin/env bash

set -eu

base="$(dirname "$(readlink -f "$0")")"
. "$base/.xlayoutlib"

fbsetroot -mod 5 5 -fg rgb:00/10/00 -bg rgb:00/00/00

out_eDP=$(find-xrandr-connected-output 'eDP-*') \
&& xinput map-to-output 'Raydium Corporation Raydium Touch System' "$out_eDP" 2>/dev/null
