#!bash
# shellcheck disable=SC2239

# enable color support of some commands and also add handy aliases
if [ "$TERM" != "dumb" ]; then
	eval "$(dircolors)"
	alias ls='ls --color=auto --group-directories-first'
	alias dir='ls --color=auto --format=vertical'
	alias vdir='ls --color=auto --format=long'
	alias grep='grep --color=auto'
	alias ncdu='ncdu --color=dark'
fi

# some more ls aliases
alias ll='ls -l'
alias la='ls -A'
alias l='ls -CF'

# Some more alias to avoid making mistakes:
alias rm='rm -i'
alias cp='cp -i'
alias mv='mv -i'

# shellcheck disable=SC2016
alias git-dotfiles='git --git-dir="$HOME/src/dotfiles.git"'
# shellcheck disable=SC2016
alias git-dotfiles-export='export GIT_DIR="$HOME/src/dotfiles.git"'
