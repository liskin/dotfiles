---
title: 'XRANDR-SMART(1) liskin/dotfiles manpages'
author: Tomas Janousek
date: October 2020
---

# NAME

xrandr-smart - wrapper around xrandr which allows shell globs in `--output`
and automatically disables all other outputs

# SYNOPSIS

`xrandr-smart [xrandr options]`

# DESCRIPTION

*`xrandr-smart`* improves the user experience of the `xrandr` command-line and
makes it easier to write generic layout scripts without harcoding output
names. It works in two steps:

1. For every `--output`, treat the argument as an extended shell glob (see
   "Pattern Matching" in `bash`(1)) and find the unique _connected_ output
   that matches. Fail otherwise.

2. For every other output (connected or disconnected) not mentioned by any
   `--output` option, append `--output out --off` to disable it.

# EXAMPLES

Vertical layout, external monitor above laptop display:

    function layout-vertical {
      xrandr-smart --output 'eDP-*' --auto \
                   --output '!(eDP-*)' --auto --above 'eDP-*'
    }

Horizontal layout, external monitor to the right of laptop display:

    function layout-horizontal {
      xrandr-smart --output 'eDP-*' --auto \
                   --output '!(eDP-*)' --auto --right-of 'eDP-*'
    }

Both external monitor/projector and laptop display showing the same:

    function layout-clone {
      xrandr-smart --output 'eDP-*' --auto \
                   --output '!(eDP-*)' --auto --same-as 'eDP-*'
    }

Disable laptop display, use external monitor only (useful for games):

    function layout-extonly {
      xrandr-smart --output '!(eDP-*)' --auto
    }

# SEE ALSO

`xrandr`(1)

<https://work.lisk.in/2020/10/11/xrandr-ux.html>
