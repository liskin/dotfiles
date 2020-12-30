#!/usr/bin/env bash

set -eu

if [[ "$*" == "default started begin -" ]]; then
	resolvectl domain virbr0 ~libvirt.
	resolvectl dns virbr0 192.168.122.1
	resolvectl dnssec virbr0 no
fi
