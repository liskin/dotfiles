#!/usr/bin/env bash

set -eu
shopt -s nullglob

# shellcheck source-path=../..
. "$HOME"/bin/.o

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
