#!bash
# shellcheck disable=SC2239

source /usr/share/doc/fzf/examples/key-bindings.bash
source /usr/share/bash-completion/completions/fzf

function __bash_history_infinite__ {
	{
		builtin fc -lnr -2147483648
		cat "${XDG_CACHE_HOME:-$HOME/.cache}/.bash_history"
	} \
	| perl -n -l -e 's/^[ \t]*//; print unless $seen{$_}++'
}

function __fzf_history__ {
	local output
	output=$(__bash_history_infinite__ |
		FZF_DEFAULT_OPTS="--height ${FZF_TMUX_HEIGHT:-40%} $FZF_DEFAULT_OPTS --scheme=history --tiebreak=index --bind=ctrl-r:toggle-sort --wrap-sign '  ↪ ' --highlight-line $FZF_CTRL_R_OPTS +m" $(__fzfcmd) --query "$READLINE_LINE"
	) || return
	READLINE_LINE=${output}
	if [ -z "$READLINE_POINT" ]; then
		echo "$READLINE_LINE"
	else
		READLINE_POINT=0x7fffffff
	fi
}

function fopen {
	# shellcheck disable=SC2048
	fzf --bind="enter:execute:xdg-open {}" --bind="change:reload: sh -c 'locate -i {q}'" ${*+--bind="start:reload: sh -c 'locate -i {q}'" --query="$*"} </dev/null
}
