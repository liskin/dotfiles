#!bash

if id -nG | grep -qw docker; then
	__docker_ps1='\[\033[01;36m\]+docker\[\033[01;32m\]'
else
	__docker_ps1=
fi

function __col1_ps1 {
	[[ $MC_SID ]] && return

	local cur_y=
	while read -t 0.0001 -r; do :; done
	IFS='[;' read -s -r -d'R' -p$'\033[6n' _ _ cur_y
	if [[ $cur_y != 1 ]]; then
		echo $'\001\033[41m↵\033[m\002\n\r'
	fi
}

PROMPT_COMMAND+=(__lastexit_save_ps1)
function __lastexit_save_ps1 { __lastexit_ps1=$?; }

PS1='$(__col1_ps1)\[\033[48;5;053m\033[01;$(( __lastexit_ps1 ? 31 : 32 ))m\][\u'${__docker_ps1}'@\h\[\033[01;36m\]$(__git_ps1 " ($(git repo-name):%s)")\[\033[01;33m\] \W]\$\[\033[00m\] '

unset __docker_ps1

## If this is an xterm set the title to user@host:dir
#case $TERM in
#xterm*|rxvt*)
#	PROMPT_COMMAND='echo -ne "\033]0;${USER}@${HOSTNAME%%.*}: ${PWD}\007"'
#	;;
#*)
#	;;
#esac
