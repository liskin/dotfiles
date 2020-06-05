#!bash

if [[ $(type -t export-aliases) == "" ]]; then
	function export-aliases { :; }
fi

# enable color support of ls and also add handy aliases
if [ "$TERM" != "dumb" ]; then
	eval "$(dircolors -b ~/.dircolors)"
	alias ls='ls --color=auto --group-directories-first'
	alias dir='ls --color=auto --format=vertical'
	alias vdir='ls --color=auto --format=long'
	alias grep='grep --color=auto'
fi

# some more ls aliases
alias ll='ls -l'
alias la='ls -A'
alias l='ls -CF'

alias nomi="ssh -C nomi.cz -t 'bash -l -c ~/bin/atmux'"
alias nomi-https="ssh -C nomi.cz-https -t 'bash -l -c ~/bin/atmux'"
alias nomi-mosh="MOSH_TITLE_NOPREFIX=1 mosh nomi.cz bin/atmux"
export-aliases nomi nomi-https nomi-mosh

alias ssh=ssh-tweaks

alias m='mutt -F ~/.muttrc-nomi'
alias m-altworx='mutt-altworx'
alias m-tmux="tmux new-session -s m -n nomi \\; new-window -d -n altworx \\; send-keys -l \$'m\\n' \\; send-keys -t :altworx -l \$'m-altworx\\n'"
export-aliases m m-altworx m-tmux

alias mutt-altworx='mutt -F ~/.muttrc-altworx'
alias mutt-news-gmane='mutt -g news.gmane.io'
alias mutt-news-kernel='mutt -g nntp.lore.kernel.org'
#alias mutt-lkml='mutt -f news://nntp.lore.kernel.org/org.kernel.vger.linux-kernel'
export-aliases mutt-altworx mutt-news-gmane mutt-news-kernel

alias wiki="vim ~/vimwiki/index.wiki"
export-aliases wiki
