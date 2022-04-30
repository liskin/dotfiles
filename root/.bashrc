#!bash
# shellcheck disable=SC2239

# ~/.bashrc: executed by bash(1) for non-login shells.
# see /usr/share/doc/bash/examples/startup-files (in the package bash-doc)
# for examples

# If not running interactively, don't do anything
case $- in
	*i*) ;;
	  *) return;;
esac

function __bashrc_d_get {
	local LC_COLLATE=C
	__bashrc_d=(~/.bashrc.d/*.sh)
}; __bashrc_d_get; unset -f __bashrc_d_get
for i in "${__bashrc_d[@]}"; do
	if [[ $__bashrc_bench ]]; then
		TIMEFORMAT="$i: %R"
		time . "$i"
		unset TIMEFORMAT
	else
		. "$i"
	fi
done; unset i; unset __bashrc_d
