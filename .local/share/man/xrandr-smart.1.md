---
title: 'XRANDR-SMART(1) liskin/dotfiles manpages'
author: Tomáš Janoušek
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
   "Pattern Matching" in `bash`(1)) — with optional disambiguation index (see
   later) — and find the unique _connected_ output that matches.
   Fail otherwise.

2. For every other output (connected or disconnected) not mentioned by any
   `--output` option, append `--output out --off` to disable it.

Disambiguation index syntax: `glob#n` selects `n`-th output matching `glob`.

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

Triangle layout, two external monitors side-by-side and laptop display beneath
them:

    function layout-triangle {
      xrandr-smart --output '!(eDP-*)#0' --mode 1920x1080 --pos 0x0 \
                   --output '!(eDP-*)#1' --mode 1920x1080 --pos 1920x0 \
                   --output   'eDP-*'    --mode 1920x1080 --pos 960x1080 --primary
    }

# SEE ALSO

`xrandr`(1)

<https://work.lisk.in/2020/10/11/xrandr-ux.html>

# COPYRIGHT

Copyright © 2021 Tomáš Janoušek

License: MIT. See <https://github.com/liskin/dotfiles/blob/home/LICENSE>.
