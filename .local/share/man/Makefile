.PHONY: all
all: man1/xrandr-smart.1

man1/xrandr-smart.1: xrandr-smart.1.md
	pandoc --standalone --to man $< -o $@
