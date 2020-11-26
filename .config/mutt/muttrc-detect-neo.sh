#!/usr/bin/env bash
set -eu
[[ $1 == */usr/share/mutt/* ]] && echo 'source ~/.config/mutt/muttrc-traditional'
[[ $1 == */usr/share/neomutt/* ]] && echo 'source ~/.config/mutt/muttrc-neomutt'
