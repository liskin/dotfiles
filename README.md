# [liskin][]'s dotfiles

[liskin]: https://github.com/liskin

These are my dotfiles, or perhaps more accurately **my personal monorepo**.
I use this with `core.worktree` set to my home directory, so no fancy dotfiles
managers or setup scripts, just plain git, and perhaps a few Makefiles here
and there.

There's also the [root](https://github.com/liskin/dotfiles/tree/root) branch,
which I use as a separate git worktree with `core.worktree = /`.

## Sub-projects

As the monorepo possibly contains stuff that'd be useful separately, there's
[a script](bin/liskin-dotfiles-release) which takes care of releasing that
into a separate branch (and tags). The following is currently available in
separate form:

 * [xrandr-smart][] - a wrapper around xrandr which allows shell globs in
   `--output` and automatically disables all other outputs;
   <https://work.lisk.in/2020/10/11/xrandr-ux.html>

[xrandr-smart]: https://github.com/liskin/dotfiles/tree/standalone/xrandr-smart

If you'd like to see another part of this repo released separately (perhaps
for packaging in a GNU/Linux distribution), let me know.
