#!sh
# ^ sourced from /etc/X11/Xsession via ~/.xsessionrc
# shellcheck disable=SC2239

export PATH=/usr/local/bin:/usr/bin:/bin:/usr/bin/X11:/usr/games
export PATH="$HOME"/.local/share/flatpak/exports/bin:/var/lib/flatpak/exports/bin:"$PATH"
export PATH="$HOME"/.cargo/bin:"$PATH"
export PATH="$HOME"/.cabal/bin:"$PATH"
export PATH="$HOME"/.local/share/mise/shims:"$PATH"
export PATH="$HOME"/.local/bin:"$PATH"
export PATH="$HOME"/bin/.ext:"$PATH"
export PATH="$HOME"/bin/.untracked:"$PATH"
export PATH="$HOME"/bin/.aliases:"$PATH"
export PATH="$HOME"/bin:"$PATH"

# manpath only
for manpath in "$HOME"/.local/share/man/.manpath/*; do
	[ -e "$manpath" ] && export PATH="$manpath":"$PATH"
done

export MAIL=/var/mail/tomi

# various hacks
export LD_LIBRARY_PATH=~/src/pango-force-subpixel-positioning/_build
export QT_AUTO_SCREEN_SCALE_FACTOR=0
export QT_QPA_PLATFORMTHEME=gtk3
export SAL_USE_VCLPLUGIN=gtk3
export _JAVA_AWT_WM_NONREPARENTING=1
export LESS="--mouse --wheel-lines=3 --LONG-PROMPT"

export BROWSER=google-chrome
export DVIVIEWER=okular
export EDITOR=nvim
export PDFVIEWER=zathura
export PSVIEWER=zathura
export SSH_ASKPASS=/usr/bin/ssh-askpass
export SUDO_ASKPASS=/usr/bin/ssh-askpass

export RIPGREP_CONFIG_PATH=~/.config/ripgreprc
export FZF_DEFAULT_COMMAND='rg --files'
export FZF_DEFAULT_OPTS='--preview-window=border-left --info=inline-right'
export FZF_TMUX_HEIGHT=100%
export FZF_TMUX=0
export BAT_STYLE=plain

export CURL_CA_BUNDLE=/etc/ssl/certs/ca-certificates.crt

export MPD_HOST="$XDG_RUNTIME_DIR"/mpd/socket

export JAVA_HOME=/usr/lib/jvm/java-11-openjdk-amd64
export JDKHOME="$JAVA_HOME"

export ANDROID_HOME=/home/tomi/android/android-sdk-linux

export DEBFULLNAME="Tomas Janousek"
export DEBEMAIL="tomi@nomi.cz"

export DEBUGINFOD_PROGRESS=1
export DEBUGINFOD_URLS="https://debuginfod.debian.net"

if [ -n "$_LISKIN_NVIDIA" ]; then
	export VK_ICD_FILENAMES="/home/tomi/.local/share/vulkan/icd.d/nvidia_icd.json"
	unset __EGL_VENDOR_LIBRARY_FILENAMES
else
	VK_ICD_FILENAMES="$(printf "%s:" /usr/share/vulkan/icd.d/intel_icd.*.json)"
	export VK_ICD_FILENAMES="${VK_ICD_FILENAMES%:}"
	export __EGL_VENDOR_LIBRARY_FILENAMES="/usr/share/glvnd/egl_vendor.d/50_mesa.json"
fi

if [ "$COLORTERM" = "rxvt-xpm" ]; then
	unset COLORTERM
fi
