#!bash

if [[ ! $DISPLAY && $(tty) == "/dev/tty10" ]]; then
	exec startx
	exit 1
fi

. ~/.bashrc
