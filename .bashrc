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
	[[ $__bashrc_bench ]] && { echo "$i"; date +%s.%N; }
	. "$i"
	[[ $__bashrc_bench ]] && { date +%s.%N; }
done; unset i
