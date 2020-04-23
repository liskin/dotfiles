#!bash

alias git-dotfiles='git --git-dir="$HOME/src/dotfiles.git"'
alias git-dotfiles-export='export GIT_DIR="$HOME/src/dotfiles.git"'
alias git-dotfiles-root='git --git-dir="$HOME/src/dotfiles.git/worktrees/dotfiles-root"'
alias git-dotfiles-root-export='export GIT_DIR="$HOME/src/dotfiles.git/worktrees/dotfiles-root"'
alias git-status-untracked='git -c status.showUntrackedFiles=normal status .'

. /usr/share/bash-completion/completions/git
complete -F _git git-dotfiles
