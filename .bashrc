#!bash

# ~/.bashrc: executed by bash(1) for non-login shells.
# see /usr/share/doc/bash/examples/startup-files (in the package bash-doc)
# for examples

# If not running interactively, don't do anything
case $- in
	*i*) ;;
	  *) return;;
esac

for i in ~/.bashrc.d/*.sh; do
	if [[ $__bashrc_bench ]]; then
		TIMEFORMAT="$i: %R"
		time . "$i"
		unset TIMEFORMAT
	else
		. "$i"
	fi
done; unset i
