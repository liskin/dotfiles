#!sh
# ^ sourced from /etc/X11/Xsession via ~/.xsessionrc
# shellcheck disable=SC2239

export PATH=/usr/local/bin:/usr/bin:/bin:/usr/bin/X11:/usr/games
export PATH=$HOME/bin/.aliases:$HOME/bin:$HOME/.local/bin:$PATH

export MAIL=/var/mail/tomi

# various hacks
export LD_LIBRARY_PATH=~/src/pango-force-subpixel-positioning/_build
export GTK_IM_MODULE=xim
export QT_AUTO_SCREEN_SCALE_FACTOR=0
export QT_QPA_PLATFORMTHEME=gtk3
export SAL_USE_VCLPLUGIN=gtk3
export XCOMPOSEFILE=/etc/X11/Compose
export _JAVA_AWT_WM_NONREPARENTING=1
export _JAVA_OPTIONS="-Dawt.useSystemAAFontSettings=lcd_hrgb -Dswing.defaultlaf=com.sun.java.swing.plaf.gtk.GTKLookAndFeel"
export LESS="--mouse --wheel-lines=3"

export BROWSER=google-chrome
export DVIVIEWER=okular
export EDITOR=vim
export PDFVIEWER=zathura
export PSVIEWER=zathura
export SSH_ASKPASS=/usr/lib/ssh/x11-ssh-askpass

export RIPGREP_CONFIG_PATH=~/.config/ripgreprc
export FZF_DEFAULT_COMMAND='rg --files'
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

if [ -n "$_LISKIN_NVIDIA_ENV" ]; then
	export VK_ICD_FILENAMES="/usr/share/vulkan/icd.d/nv_vulkan_wrapper.json"
	unset __EGL_VENDOR_LIBRARY_FILENAMES
else
	export __EGL_VENDOR_LIBRARY_FILENAMES="/usr/share/glvnd/egl_vendor.d/50_mesa.json"
fi
