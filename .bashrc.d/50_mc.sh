#!bash
# shellcheck disable=SC2239

unalias mc 2>/dev/null
function mc {
	[[ $XDG_RUNTIME_DIR ]] || return
	mkdir -p "${XDG_RUNTIME_DIR}/mc"
	local MC_PWD_FILE="${XDG_RUNTIME_DIR}/mc/mc.pwd.$$"
	local MC_X=()
	if [[ "$(tput longname)" == *xterm* ]]; then
		MC_X=("-x")
	fi
	/usr/bin/mc "${MC_X[@]}" -P "$MC_PWD_FILE" "$@"
	if [[ -r "$MC_PWD_FILE" ]]; then
		local MC_PWD
		MC_PWD="$(< "$MC_PWD_FILE")"
		if [[ $MC_PWD && -d $MC_PWD ]]; then
			cd "$MC_PWD" || :
		fi
	fi
	rm -f "$MC_PWD_FILE"
}
