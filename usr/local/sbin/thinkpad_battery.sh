#!/usr/bin/env bash

set -eu

case "$1" in
	charge)
		echo 70 > /sys/class/power_supply/BAT0/charge_start_threshold
		echo 70 > /sys/class/power_supply/BAT1/charge_start_threshold
		echo 80 > /sys/class/power_supply/BAT0/charge_stop_threshold
		echo 80 > /sys/class/power_supply/BAT1/charge_stop_threshold
		;;

	normal)
		echo 40 > /sys/class/power_supply/BAT0/charge_start_threshold
		echo 40 > /sys/class/power_supply/BAT1/charge_start_threshold
		echo 80 > /sys/class/power_supply/BAT0/charge_stop_threshold
		echo 80 > /sys/class/power_supply/BAT1/charge_stop_threshold
		;;

	full-int)
		echo 100 > /sys/class/power_supply/BAT0/charge_stop_threshold
		echo 90 > /sys/class/power_supply/BAT0/charge_start_threshold
		;;

	full-ext)
		echo 100 > /sys/class/power_supply/BAT1/charge_stop_threshold
		echo 90 > /sys/class/power_supply/BAT1/charge_start_threshold
		;;

	full)
		echo 100 > /sys/class/power_supply/BAT0/charge_stop_threshold
		echo 100 > /sys/class/power_supply/BAT1/charge_stop_threshold
		echo 90 > /sys/class/power_supply/BAT0/charge_start_threshold
		echo 90 > /sys/class/power_supply/BAT1/charge_start_threshold
		;;
esac
