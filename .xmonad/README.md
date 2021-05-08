# My [xmonad][] setup

[xmonad]: https://xmonad.org/

<p>
<img alt="laptop screenshot" src="https://user-images.githubusercontent.com/300342/111490986-c0a82f80-873b-11eb-8509-00330bfc0aa7.png" width="660">
<img alt="home workspace" src="https://user-images.githubusercontent.com/300342/111066035-3bafe280-84bd-11eb-97eb-d5abf9acf5d8.jpg" width="330">
</p>

goals: high density, high contrast, low friction, low latency;
non-goals: eye candy

## Overview

* 24 workspaces named 1…12, W1…W12, bound to Alt-F1…12, Win-F1…F2

* dynamic workspace names/descriptions using [X.A.WorkspaceNames][]; dynamic
  per-workspace workdirs using [X.L.WorkspaceDir][]; tied together with
  [~/bin/rofi-git-all-repos](../bin/rofi-git-all-repos)
  ([screenshot][rofi-screenshot]) to get dynamic topic/project workspaces

* two most used layouts: [X.L.ResizableTile][] with tabs using
  [X.L.SubLayouts][] (minimal decoration, window titles are in xmobar);
  Full (with window titles in xmobar this resembles fullscreen tabbed)

* top [xmobar][] on primary screen shows workspaces, [weechat][]
  notifications, urgents on the left; batteries, temperature, CPUs, net
  throughput and datetime on the right; commonly used words are replaced with
  icons to increase density

* bottom [xmobar][] on each screen shows layout, workdir and window titles
  (like a tmux status line or screen hardstatus); plus [trayer][] on primary
  screen

* [do-not-disturb mode](XMonad/Actions/DoNotDisturb.hs) that hides chat
  notifications and defers urgency requests; together with an xmobar widget
  and [a timer](../bin/liskin-xmonad-dnd-timer) (started from
  [xmonadctl](../bin/xmonadctl)) this acts as a "pomodoro timer"

* program launching, layout selection, emoji typing, password typing via
  [rofi][] (see [bindings](xmonad.hs) and scripts in [`~/bin`](../bin))

* media keys bound to [liskin-media](../bin/liskin-media), a script that
  [handles controlling the right media player][linux-media-control] and
  bluetooth volume

* screen locking via [xss-lock][] + [xsecurelock][] (more robust than
  xscreensaver, integrated with systemd-logind, can handle media keys)

* xrandr automation via [layout-auto](../bin/layout-auto) and [xrandr-smart][]
  (`xrandr-smart --output 'eDP-*' --auto --output '!(eDP-*)' --auto --above 'eDP-*'`)

* automatic time tracking via [arbtt][] (guided by workspace names, see
  [categorize.cfg](../.arbtt/categorize.m4)), visualisaton via
  [arbtt-chart][], [daily summary in mail][arbtt-chart-mail]

[X.A.WorkspaceNames]: https://hackage.haskell.org/package/xmonad-contrib/docs/XMonad-Actions-WorkspaceNames.html
[X.L.WorkspaceDir]: https://hackage.haskell.org/package/xmonad-contrib/docs/XMonad-Layout-WorkspaceDir.html
[X.L.ResizableTile]: https://hackage.haskell.org/package/xmonad-contrib/docs/XMonad-Layout-ResizableTile.html
[X.L.SubLayouts]: https://hackage.haskell.org/package/xmonad-contrib/docs/XMonad-Layout-SubLayouts.html
[rofi]: https://github.com/davatorium/rofi
[linux-media-control]: https://work.lisk.in/2020/05/06/linux-media-control.html
[xmobar]: https://github.com/jaor/xmobar
[xss-lock]: https://salsa.debian.org/liskin/xss-lock
[xsecurelock]: https://github.com/google/xsecurelock
[xrandr-smart]: https://work.lisk.in/2020/10/11/xrandr-ux.html
[trayer]: https://github.com/sargon/trayer-srg
[arbtt]: http://arbtt.nomeata.de/
[arbtt-chart]: https://github.com/liskin/arbtt-chart
[rofi-screenshot]: https://user-images.githubusercontent.com/300342/111072646-b851b980-84db-11eb-9b92-b8e3024c0032.png
[arbtt-chart-mail]: https://user-images.githubusercontent.com/300342/111073951-2a78cd00-84e1-11eb-9483-5b2120a9ad77.png
[weechat]: https://weechat.org/

## Sources

* git submodules in [`~/src-haskell`](../src-haskell): mostly my forks with a
  `dotfiles` branch that has some extra stuff that hasn't been upstreamed yet

* haskell modules in [`~/.xmonad/XMonad`](XMonad): self-contained additions
  not suitable for upstreaming or still work-in-progress

* [`~/.xmonad/xmonad.hs`](xmonad.hs): xmonad config main

## Build

Built using [stack][] and [make][]:

* [`~/src-haskell/Makefile`](../src-haskell/Makefile): dependencies (xmonad,
  xmonad-contrib, xmobar, X11, …) built by `stack build`; utilities
  (xmonadctl, xmonadpropwrite, …) built by `stack exec ghc`

* [`~/.xmonad/Makefile`](Makefile): xmonad config binary built by `stack exec
  ghc`; helper targets for ghci/[ghcid][]

* no automatic recompile: [invoked directly via the `xmonad-ARCH-OS`
  binary](../bin/xmonad), built manually by typing `make`

[stack]: https://docs.haskellstack.org/en/stable/README/
[make]: https://www.gnu.org/software/make/
[ghcid]: https://github.com/ndmitchell/ghcid

## X desktop “manager” and X session

TL;DR: `startx` in `.bash_profile`, xmonad as `graphical-session.target` in
user systemd, support for multiple X sessions (primary on intel, temporary on
nvidia dGPU).

Details:

* [`.bash_profile`](../.bash_profile) sources [`.bash_profile_xdm`](../.bash_profile_xdm)

* [`.bash_profile_xdm`](../.bash_profile_xdm): if `XDG_SESSION_TYPE=x11`,
  `exec startx /etc/X11/xinit/xinitrc ~/.xsession-wrapper` (secondary X
  sessions skip the `/etc/X11/xinit/xinitrc` to avoid messing up user systemd
  and dbus environments)

* [gettys 7 to 10 set
  `XDG_SESSION_TYPE=x11`](../../root/etc/systemd/system/getty@tty10.service.d/override.conf)

* [getty 9 additionally sets
  `_LISKIN_NVIDIA=1`](../../root/etc/systemd/system/getty@tty9.service.d/override.conf)
  so that a different [`xorg.conf`](../../root/etc/X11/xorg.conf.nvidia) is used
  and everything is rendered on the dGPU

* [`.xsession-wrapper`](../.xsession-wrapper) invokes
  [`.xsession`](../.xsession) with stdout/stderr to systemd journal

* [`.xsession`](../.xsession) performs some initialization (xrdb, xrandr, xset,
  xsetroot, setxkbmap, xmodmap, xinput, pulseaudio, …) and then launches
  xmonad via systemd-run: in primary sessions as a unit binding to
  [`graphical-session.target`](https://www.freedesktop.org/software/systemd/man/systemd.special.html#graphical-session.target)
  and [`xmonad@$DISPLAY.target`](../.config/systemd/user/xmonad@.target), in
  secondary sessions as a scope binding to `xmonad@$DISPLAY.target`

* `graphical-session.target` starts [primary session
  services](../.config/systemd/user/graphical-session.target.wants) like tray
  applets, notification daemon, …

* [`xmonad@$DISPLAY.target`](../.config/systemd/user/xmonad@.target) starts
  services meant to run in all sessions (compton, xss-lock, redshift, …)

## See also

* there may be other interesting (but likely undocumented) bits in [my
  dotfiles](https://github.com/liskin/dotfiles/)

* my mostly technical [blog/web](https://work.lisk.in/);
  [@Liskni\_si twitter](https://twitter.com/Liskni_si)

* [support my free software activities via GitHub
  Sponsors](https://github.com/sponsors/liskin)
