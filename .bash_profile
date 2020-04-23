#!bash

if [[ ! $DISPLAY && $(tty) == "/dev/tty10" ]]; then
	. ~/.bashrc.d/10_env.sh
	mv -f ~/.xsession-errors ~/.xsession-errors.old
	exec startx /etc/X11/xinit/xinitrc
	exit 1
fi

. ~/.bashrc
