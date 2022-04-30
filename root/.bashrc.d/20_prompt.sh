#!bash
# shellcheck disable=SC2239

# newline if not in first column
function __col1_ps1 {
	[[ $MC_SID ]] && return

	local termios cur_y
	# ask the terminal for any pending (line buffered) input
	termios=$(stty --save) && stty -icanon && stty "$termios"
	# if there's pending input, assume it's been echoed and we're not in first column
	# otherwise ask the terminal for current column and read it from input
	if read -t 0 || { IFS='[;' read -s -r -d'R' -p$'\033[6n' _ _ cur_y && [[ $cur_y != 1 ]]; }; then
		echo $'\001\033[41m↵\033[m\002\n\001\r\002'
	fi
}

PROMPT_COMMAND+=(__lastexit_save_ps1)
# shellcheck disable=SC2034
function __lastexit_save_ps1 { __lastexit_ps1=$?; }

PS1='$(__col1_ps1)\[\033[48;5;088m\033[01;$(( __lastexit_ps1 ? 31 : 32 ))m\][\u@\h\[\033[01;36m\]$(__git_ps1 " ($(git repo-name):%s)")\[\033[01;33m\] \W]#\[\033[00m\] '
