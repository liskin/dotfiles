#!bash

cmds_required=(
	bwrap
	fzf
	getcap
	jq
	setcap
)
files_required=(
	/etc/profile.d/bash_completion.sh
)
pkgs_required=(
	bash-completion
	bubblewrap
	fzf
	jq
	libcap2-bin
)
if ! has-cmds "${cmds_required[@]}" || ! has-files "${files_required[@]}"; then
	oo "# apt update" # sources.list may have changed
	oo "# apt install ${pkgs_required[*]}"
fi

oo "# bash -l -c 'make -B'"
