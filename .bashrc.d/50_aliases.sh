#!bash
# shellcheck disable=SC2239

if [[ $(type -t export-alias) == "" ]]; then
	function export-alias { builtin alias "$@"; }
fi

# enable color support of some commands and also add handy aliases
if [ "$TERM" != "dumb" ]; then
	eval "$(dircolors -b ~/.dircolors)"
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

# extend some commands with extra functionality
alias ssh=ssh-tweaks
alias task=task-local

alias urxvt-setfont-anonymous-pro='urxvt-setfont "Anonymous Pro"'
alias urxvt-setfont-cascadia-mono='urxvt-setfont "Cascadia Mono"'
alias urxvt-setfont-courier-prime='urxvt-setfont "Courier Prime Code"'
alias urxvt-setfont-fira-code='urxvt-setfont "Fira Code"'
alias urxvt-setfont-hack='urxvt-setfont "Hack"'
alias urxvt-setfont-inconsolata='urxvt-setfont "Inconsolata"'
alias urxvt-setfont-iosevka-fixed='urxvt-setfont "Iosevka Fixed SS11"'
alias urxvt-setfont-monospace='urxvt-setfont monospace 10'

export-alias nomi="ssh -C nomi.cz -t 'bash -l -c ~/bin/atmux'"
export-alias nomi-https="ssh -C nomi.cz-https -t 'bash -l -c ~/bin/atmux'"
export-alias nomi-mosh="MOSH_TITLE_NOPREFIX=1 mosh nomi.cz bin/atmux"

export-alias mutt='LC_MESSAGES=en_US.UTF-8 /usr/bin/mutt'
export-alias neomutt='LC_MESSAGES=en_US.UTF-8 /usr/bin/neomutt'

export-alias mutt-nomi='mutt -F ~/.config/mutt/muttrc-nomi'
export-alias neomutt-nomi='neomutt -F ~/.config/mutt/muttrc-nomi'

export-alias m='mutt-nomi'
export-alias m-tmux="tmux new-session -s m -n nomi \\; new-window -d -n local \\; send-keys -l \$'m\\n' \\; send-keys -t :local -l \$'mutt\\n'"

export-alias mutt-news-gmane='neomutt -g news.gmane.io'
export-alias mutt-news-kernel='neomutt -g nntp.lore.kernel.org'
#alias mutt-lkml='mutt -f news://nntp.lore.kernel.org/org.kernel.vger.linux-kernel'

export-alias wiki="vim ~/taskwiki/index.wiki"

# shellcheck disable=SC2016
export-alias git-dotfiles='git --git-dir="$HOME/src/dotfiles.git"'
alias git-dotfiles-export='export GIT_DIR="$HOME/src/dotfiles.git"'

export-alias livereload-make="python3 -c 'from livereload import Server, shell; server = Server(); server.watch(\".\", shell(\"make\")); server.serve();'"

export-alias google-chrome-app-diagrams="gtk-launch chrome-ilmgmogedobmcfegdjcibiiaodmdenpf-Default"
export-alias google-chrome-app-discord="google-chrome --app=https://discord.com/app"
export-alias google-chrome-app-excalidraw="gtk-launch chrome-kmcnjdcipmgchfndpapkoecbidofdpbc-Default"
export-alias google-chrome-app-fb-messenger="google-chrome --app=https://www.messenger.com/"
export-alias google-chrome-app-google-chat="google-chrome --app=https://chat.google.com/"
export-alias google-chrome-app-keep="google-chrome --app=https://keep.google.com/"
export-alias google-chrome-app-matrix-element="google-chrome --app=https://app.element.io/"
export-alias google-chrome-app-skype="google-chrome --app=https://web.skype.com/"
export-alias google-chrome-app-wire="google-chrome --app=https://app.wire.com/"

export-alias google-chrome-profile-mitm="bwrap-pki-mitm google-chrome-profile"

export-alias gitg='LC_MESSAGES=C /usr/bin/gitg'
export-alias meld='LC_MESSAGES=C /usr/bin/meld'
export-alias weechat='LC_MESSAGES=C /usr/bin/weechat'
export-alias cantata='LC_COLLATE=C /usr/bin/cantata'

export-alias google-earth-pro='LC_NUMERIC=C /usr/bin/google-earth-pro'
export-alias google-earth='google-earth-pro'

export-alias nmcli-c-up-bluetooth='nmcli c up s10e.phone.lisk.in'
export-alias nmcli-c-down-bluetooth='nmcli c down s10e.phone.lisk.in'
export-alias bt-connect-qc45='bluetoothctl connect 78:2B:64:A0:68:02'
export-alias bt-disconnect-qc45='bluetoothctl disconnect 78:2B:64:A0:68:02'
export-alias bt-connect-buds='bluetoothctl connect B0:4A:6A:60:5E:A7'
export-alias bt-disconnect-buds='bluetoothctl disconnect B0:4A:6A:60:5E:A7'

export-alias mount-dropbox='systemctl --user start liskin-rclone-dropbox.service'
export-alias umount-dropbox='systemctl --user stop liskin-rclone-dropbox.service'

export-alias rofi-emoji-menu='EMOJI_FONT="emoji mono" ~/src/emoji-rofi-menu/emoji-menu.sh'
export-alias rofi-emoji-sign='EMOJI_FONT="emoji mono" ~/src/emoji-rofi-menu/emoji-sign.sh'
export-alias rofi-window='rofi -modi combi -combi-modi "window,browser:~/bin/rofi-plasma-browser-integration" -show combi'

export-alias steam='bwrap-steam /usr/games/steam'
export-alias protontricks='bwrap-steam /usr/bin/protontricks'

export-alias arbtt=liskin-arbtt-stats
export-alias arbtt-week='arbtt activity-chart ^sow tomorrow'

alias paste-x0at='curl -F "file=@-" https://x0.at/; echo'
alias paste-sprungeus='curl -F "sprunge=<-" http://sprunge.us; echo'

# /dev/rfkill is tagged uaccess in udev
export-alias rfkill=/usr/sbin/rfkill

# https://bugs.debian.org/cgi-bin/bugreport.cgi?bug=940511
export-alias yarn=yarnpkg

alias urldecode='python3 -c "import sys; from urllib.parse import unquote; from functools import reduce; reduce(lambda _, __: None, map(lambda l: print(unquote(l.rstrip())), sys.stdin));"'
alias urlencode='python3 -c "import sys; from urllib.parse import quote; from functools import reduce; reduce(lambda _, __: None, map(lambda l: print(quote(l.rstrip())), sys.stdin));"'
