#!bash
# shellcheck disable=SC2239

function cdgit {
	local dir
	dir=$(git-all-repos | fzf-tmux --read0) || return

	if [[ -n $dir && -d $dir ]]; then
		cd "$dir" || return
	fi
}

function sshfs-slave {
	local -r host="${1:?}"
	local -r localPath="${2:?}"
	local -r remotePath="${3:?}"

	socat EXEC:/usr/lib/openssh/sftp-server,pipes EXEC:"ssh $host sshfs -o slave none\:$localPath $remotePath",nofork
}

function sshfs-slave-readonly {
	local -r host="${1:?}"
	local -r localPath="${2:?}"
	local -r remotePath="${3:?}"

	socat EXEC:'/usr/lib/openssh/sftp-server -R',pipes EXEC:"ssh $host sshfs -o slave none\:$localPath $remotePath",nofork
}

. /usr/share/bash-completion/completions/ssh
complete -F _ssh sshfs-slave
complete -F _ssh sshfs-slave-readonly

function whiteboardclean {
	convert "$1" -morphology Convolve DoG:15,100,0 -negate -normalize -blur 0x1 -channel RBG -level 60%,91%,0.1 "$2"
}

function uplfile {
	(( $# )) || return

	rsync --chmod=F644 -v "$@" nomi.cz:~/wwwtmp/ || return

	local f x
	for f in "$@"; do
		x=$(basename "$f" | perl -MURI::Escape -pe 's|.*|uri_escape($&)|e')
		echo https://store.lisk.in/tmp/"$x"
	done
}

function uplfileperm {
	(( $# )) || return

	rsync --chmod=F644 -v "$@" nomi.cz:~/wwwtmp/perm/ || return

	local f x
	for f in "$@"; do
		x=$(basename "$f" | perl -MURI::Escape -pe 's|.*|uri_escape($&)|e')
		echo https://store.lisk.in/tmp/perm/"$x"
	done
}

function rg {
	if [[ -t 0 && -t 1 && -t 2 ]]; then
		command rg -p "$@" | less -FRX
	else
		command rg "$@"
	fi
}

function mise {
	unset GIT_DIR  # prevent git-dotfiles-export messing up stuff

	GITHUB_API_TOKEN=$(keyring get gh:github.com '') \
		command mise "$@"
}
function uv {
	unset GIT_DIR  # prevent git-dotfiles-export messing up stuff
	command uv "$@"
}
