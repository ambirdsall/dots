export DOTPROFILE_HAS_RUN=t

if command -v xcape > /dev/null; then
  pgrep xcape &> /dev/null || xcape -e 'Control_L=Escape;Shift_L=Shift_L|9;Shift_R=Shift_R|0'
fi

# TODO some automated scripts
if command -v xcape > /dev/null; then
  if [ -z $XMODMAP_SET ]; then xmodmap ~/.Xmodmap; fi
  export XMODMAP_SET=t
fi

if command -v xfce4-terminal > /dev/null; then
    export TERMINAL=xfce4-terminal
fi

#export QT_QPA_PLATFORMTHEME=qt5ct
#export QT_QPA_PLATFORMTHEME=qt6ct

# set PATH so it includes user's private bin if it exists
if [ -d "$HOME/.local/bin" ] ; then
    export PATH="$HOME/.local/bin:$PATH"
fi

if [ -d "$HOME/.yarn/bin" ] ; then
    export PATH="$HOME/.yarn/bin:$PATH"
fi

if [ -d "$HOME/.cargo/bin" ] ; then
    export PATH="$HOME/.cargo/bin:$PATH"
fi

# set PATH so it includes user's private bin if it exists
if [ -d "$HOME/bin" ] ; then
    export PATH="$HOME/bin:$PATH"
fi

# a few barebones aliases, for muscle memory's sake
if command -v vim > /dev/null; then
    vi () {
        if [[ $# -gt 0 ]]; then
            vim "$@"
        else
            vim .
        fi
    }
fi
if command -v emacs > /dev/null; then
    em () {
        emacsclient -nw --alternate-editor=vim ${@}
    }
fi

if [ -s "$HOME/.guix-profile" ]; then
    GUIX_PROFILE="$HOME/.guix-profile"
    GUIX_LOCPATH="$GUIX_PROFILE/lib/locale"
    source "$GUIX_PROFILE/etc/profile"
fi

[ -s "$HOME/.cargo/env" ] && source "$HOME/.cargo/env"

if [ -f .zprofile.local.zsh ]; then
  source .zprofile.local.zsh
fi
