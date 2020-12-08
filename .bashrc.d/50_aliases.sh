#!bash

if [[ $(type -t export-alias) == "" ]]; then
	function export-alias { builtin alias "$@"; }
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

export-alias nomi="ssh -C nomi.cz -t 'bash -l -c ~/bin/atmux'"
export-alias nomi-https="ssh -C nomi.cz-https -t 'bash -l -c ~/bin/atmux'"
export-alias nomi-mosh="MOSH_TITLE_NOPREFIX=1 mosh nomi.cz bin/atmux"

alias ssh=ssh-tweaks

export-alias mutt='LC_MESSAGES=en_US.UTF-8 /usr/bin/mutt'
export-alias neomutt='LC_MESSAGES=en_US.UTF-8 /usr/bin/neomutt'

export-alias mutt-nomi='mutt -F ~/.config/mutt/muttrc-nomi'
export-alias neomutt-nomi='neomutt -F ~/.config/mutt/muttrc-nomi'
export-alias mutt-altworx='mutt -F ~/.config/mutt/muttrc-altworx'
export-alias neomutt-altworx='neomutt -F ~/.config/mutt/muttrc-altworx'

export-alias m='mutt-nomi'
export-alias m-altworx='mutt-altworx'
export-alias m-tmux="tmux new-session -s m -n nomi \\; new-window -d -n altworx \\; send-keys -l \$'m\\n' \\; send-keys -t :altworx -l \$'m-altworx\\n'"

export-alias mutt-news-gmane='neomutt -g news.gmane.io'
export-alias mutt-news-kernel='neomutt -g nntp.lore.kernel.org'
#alias mutt-lkml='mutt -f news://nntp.lore.kernel.org/org.kernel.vger.linux-kernel'

export-alias wiki="vim ~/taskwiki/index.wiki"

export-alias livereload-make="python3 -c 'from livereload import Server, shell; server = Server(); server.watch(\".\", shell(\"make\")); server.serve();'"

export-alias google-chrome-app-diagrams="gtk-launch chrome-pebppomjfocnoigkeepgbmcifnnlndla-Default"
export-alias google-chrome-app-fb-messenger="google-chrome --app=https://www.messenger.com/"
export-alias google-chrome-app-keep="google-chrome --app=https://keep.google.com/"
export-alias google-chrome-app-matrix-element="google-chrome --app=https://app.element.io/"
export-alias google-chrome-app-slack-altworx="google-chrome --app=https://altworx.slack.com/"
export-alias google-chrome-app-slack-goodalumni="google-chrome --app=https://goodalumni.slack.com/"

export-alias zoom='flatpak run us.zoom.Zoom'

export-alias gitg='LC_MESSAGES=C /usr/bin/gitg'
export-alias meld='LC_MESSAGES=C /usr/bin/meld'
export-alias google-earth-pro='LC_NUMERIC=C /usr/bin/google-earth-pro'
export-alias google-earth='google-earth-pro'

export-alias nmcli-c-up-bluetooth='nmcli c up s10e.phone.lisk.in'
export-alias nmcli-c-down-bluetooth='nmcli c down s10e.phone.lisk.in'
export-alias nmcli-c-up-vpn-altworx='nmcli c up vpn-altworx'
export-alias nmcli-c-down-vpn-altworx='nmcli c down vpn-altworx'
