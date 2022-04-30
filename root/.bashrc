# ~/.bashrc: executed by bash(1) for non-login shells.

export PS1='\[\033[48;5;088m\033[01;$(( $? ? 31 : 32 ))m\][\u@\h \[\033[01;33m\]\W]#\[\033[00m\] '
umask 022
#export LESS="-c"

# You may uncomment the following lines if you want `ls' to be colorized:
export LS_OPTIONS='--color=auto'
eval "`dircolors`"
alias ls='ls $LS_OPTIONS'
alias ll='ls $LS_OPTIONS -l'
alias l='ls $LS_OPTIONS -lA'
#
# Some more alias to avoid making mistakes:
alias rm='rm -i'
alias cp='cp -i'
alias mv='mv -i'

export HISTFILESIZE=50000
export HISTSIZE=50000
set +o histexpand

alias git-dotfiles='git --git-dir="$HOME/src/dotfiles.git"'
alias git-dotfiles-export='export GIT_DIR="$HOME/src/dotfiles.git"'
