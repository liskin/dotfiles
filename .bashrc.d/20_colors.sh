#!sh
# shellcheck disable=SC2239
# shellcheck disable=SC1003

# inspired by https://github.com/chriskempson/base16-shell

if [ -n "$TMUX" ]; then
  # Tell tmux to pass the escape sequences through
  # (Source: http://permalink.gmane.org/gmane.comp.terminal-emulators.tmux.user/1324)
  _set_term_color() { printf '\033Ptmux;\033\033]4;%d;rgb:%s\033\033\\\033\\' "$@"; }
elif [ "${TERM%%[-.]*}" = "screen" ]; then
  # GNU screen (screen, screen-256color, screen-256color-bce)
  _set_term_color() { printf '\033P\033]4;%d;rgb:%s\007\033\\' "$@"; }
elif [ "${TERM%%-*}" = "linux" ]; then
  _set_term_color() { printf "\e]P%x%s" "$1" "$(echo "$2" | sed 's/\///g')"; }
else
  _set_term_color() { printf '\033]4;%d;rgb:%s\033\\' "$@"; }
fi

_set_term_color 4 50/80/cd
_set_term_color 12 50/80/ff

unset _set_term_color
