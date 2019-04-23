#!bash

if [[ ! $DISPLAY && $(tty) == "/dev/tty10" ]]; then
	exec startx /etc/X11/xinit/xinitrc
	exit 1
fi

. ~/.bashrc
