#!bash
# shellcheck disable=SC2239

alias reboot='sudo reboot'
alias halt='sudo halt'
alias poweroff='sudo poweroff'

alias sudo-docker='sudo -g docker --preserve-env newgrp $(id -n -g)'
alias sudo-libvirt='sudo -g libvirt --preserve-env newgrp $(id -n -g)'
alias docker-stop='sudo systemctl stop docker.service containerd.service && sudo systemctl start docker.socket'

alias liskin-battery-graph='sudo /usr/local/sbin/liskin-battery-graph'
