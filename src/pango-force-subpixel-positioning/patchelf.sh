#!/usr/bin/env bash

set -eux

if [[ "$(ldd "$1")" =~ (libpango[^[:space:]]*)" => "(/[^[:space:]]*/libpango[^[:space:]]*) ]]; then
	soname="${BASH_REMATCH[1]}"
	lib="${BASH_REMATCH[2]}"

	patchelf --replace-needed "$soname" "orig-$soname" "$1"
	patchelf --set-rpath "\$ORIGIN" "$1"
	ln -sf "$lib" "$(dirname "$1")/orig-$soname"
	ln -sf "$(basename "$1")" "$(dirname "$1")/$soname"
fi
