#!/usr/bin/env bash

set -eu
shopt -s nullglob

function o { printf -->&2 "%s:%s\\n" "${0##*/}" "$(printf " %q" "$@")"; "$@"; }

function export-alias {
	for a in "$@"; do
		o ln -sf ../exec-alias ~/bin/.aliases/"${a%%=*}"
	done
}

o rm -f ~/bin/.aliases/*
for i in ~/.bashrc.d/5*_aliases*.sh; do
	o . "$i"
done

o touch ~/bin/.aliases/.done
