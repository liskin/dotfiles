#!/usr/bin/env bash

set -eu

tmpdir=$(mktemp -d)
# shellcheck disable=SC2064
trap "rm -rf $(printf %q "$tmpdir")" EXIT

mkfifo "$tmpdir/status"

cat >"$tmpdir/run" <<END
ret=1
trap 'echo \$ret > "$tmpdir/status"' EXIT
\${EDITOR:?} "\$@"
ret=\$?
END

if [[ ${STY-} ]]; then
	screen -X screen /usr/bin/env bash "$tmpdir/run" "$@"
elif [[ ${TMUX-} ]]; then
	tmux new-window /usr/bin/env bash "$tmpdir/run" "$@"
else
	exit 1
fi

read -r ret <"$tmpdir/status"
exit "$ret"
