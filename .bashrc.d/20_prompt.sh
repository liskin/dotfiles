#!bash

if id -nG | grep -qw docker; then
	__docker_ps1='\[\033[01;36m\]+docker\[\033[01;32m\]'
else
	__docker_ps1=
fi

PS1='\[\033[48;5;053m\033[01;$(( $? ? 31 : 32 ))m\][\u'${__docker_ps1}'@\h\[\033[01;36m\]$(__git_ps1 " ($(git repo-name):%s)")\[\033[01;33m\] \W]\$\[\033[00m\] '

unset __docker_ps1

## If this is an xterm set the title to user@host:dir
#case $TERM in
#xterm*|rxvt*)
#	PROMPT_COMMAND='echo -ne "\033]0;${USER}@${HOSTNAME%%.*}: ${PWD}\007"'
#	;;
#*)
#	;;
#esac
