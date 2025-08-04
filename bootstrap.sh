#!/usr/bin/env bash

# =============================================================================
#
# cd ~
# wget https://raw.githubusercontent.com/liskin/dotfiles/home/bootstrap.sh
# chmod +x bootstrap.sh
# ./bootstrap.sh
#
# =============================================================================

set -eu -o pipefail
shopt -s lastpipe

## include bin/.o
#!bash
# shellcheck disable=SC2239

# shellcheck disable=SC2218,SC2120
exec {_o_stderr}>&2
function exec { if (( $# )); then builtin exec {_o_stderr}>&- "$@"; else builtin exec "$@"; fi; }

if [[ -t $_o_stderr ]]; then _o_tput_bold=$(tput bold || :); _o_tput_reset=$(tput sgr0 || :); else _o_tput_bold=; _o_tput_reset=; fi
function o { printf -->&$_o_stderr "%s%s:%s%s\\n" "$_o_tput_bold" "${0##*/}" "$_o_tput_reset" "$(printf " %q" "$@")"; "$@"; }
function oo { printf -->&$_o_stderr "%s%s:%s %s\\n" "$_o_tput_bold" "${0##*/}" "$_o_tput_reset" "$*"; }
## end include bin/.o

function has-cmds { for cmd in "$@"; do type "$cmd" &>/dev/null || return; done }
function has-files { for file in "$@"; do [[ -e "$file" ]] || return; done }

cmds_essential=(
	git
	make
)
pkgs_essential=(
	build-essential
	git
)
if ! has-cmds "${cmds_essential[@]}"; then
	oo "# apt update"
	oo "# apt install ${pkgs_essential[*]}"
	exit 1
fi

[[ "$(id -u)" == 0 ]] && root=: || root=
[[ "${CODESPACES-}" == true ]] && codespaces=: || codespaces=

if [[ $root ]]; then
	branch=root; worktree=/
elif [[ $codespaces ]]; then
	branch=codespaces; worktree=~
else
	branch=home; worktree=~
fi

dotfiles=~/src/dotfiles.git
[[ -e "$dotfiles" ]] && o rm -rI "$dotfiles"
o mkdir -p ~/src
o git clone --no-checkout -b "$branch" --separate-git-dir="$dotfiles" https://github.com/liskin/dotfiles.git "$(mktemp -d)"

function git-dotfiles { git -C "$dotfiles" "$@"; }

o git-dotfiles config core.worktree "$worktree"
o git-dotfiles config status.showUntrackedFiles no
o git-dotfiles config alias.clean false
o git-dotfiles config branch."$branch".rebase true

missing=()
o git-dotfiles reset
if [[ $codespaces ]]; then
	o git-dotfiles checkout -f
else
	o git-dotfiles diff --no-renames --name-only --diff-filter=D --line-prefix="${worktree%/}/" -z | readarray -d '' missing
	o git-dotfiles checkout "${missing[@]}"
	o git-dotfiles checkout -p
fi
o git-dotfiles submodule update --init

if [[ -e ~/bootstrap-phase2.sh ]]; then
	# shellcheck disable=SC1090
	. ~/bootstrap-phase2.sh
fi
