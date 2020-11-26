#!/usr/bin/env bash
set -eu
[[ $1 == */usr/share/mutt/* ]] && echo 'set my_mutt=traditional'
[[ $1 == */usr/share/neomutt/* ]] && echo 'set my_mutt=neomutt'
