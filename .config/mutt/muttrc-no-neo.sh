#!/usr/bin/env bash
set -eu
[[ ${__neomutt-} ]] || echo 'source ~/.config/mutt/muttrc-no-neo'
