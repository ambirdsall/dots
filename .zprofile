# adds the directory to $PATH *iff*:
#   1) the directory exists
#   2) the directory is not in $PATH already
# New directories are prepended, so the most recent additions take precedence. No effort
# is taken to escape extended grep metacharacters, so if your directory name can't be used
# verbatim to match itself, you're going to have to do that one the old-fashioned way.
_add_to_path () {
    local BIN_DIR="$1"
    if [ -d "$BIN_DIR" -a ! $(echo $PATH | grep "$BIN_DIR(:|$)") ]; then
        export PATH="$BIN_DIR:$PATH"
    fi
}

_at_hand () {
    command -v $1 > /dev/null
}

if _at_hand xcape; then
  pgrep xcape &> /dev/null || xcape -e 'Control_L=Escape;Shift_L=Shift_L|9;Shift_R=Shift_R|0'
fi

# TODO some automated scripts
if _at_hand xmodmap; then
  xmodmap ~/.Xmodmap
fi

if _at_hand kitty; then
    export TERMINAL=kitty
elif _at_hand alacritty; then
    export TERMINAL=alacritty
elif _at_hand xfce4-terminal; then
    export TERMINAL=xfce4-terminal
fi

#export QT_QPA_PLATFORMTHEME=qt5ct
#export QT_QPA_PLATFORMTHEME=qt6ct

_add_to_path "$HOME/.yarn/bin"
# if [ -d "$HOME/.yarn/bin" ] ; then
#     export PATH="$HOME/.yarn/bin:$PATH"
# fi

_add_to_path "$HOME/.cargo/bin"
# if [ -d "$HOME/.cargo/bin" ] ; then
#     export PATH="$HOME/.cargo/bin:$PATH"
# fi

[ -s "$HOME/.cargo/env" ] && source "$HOME/.cargo/env"

# set PATH so it includes user's private bin if it exists
_add_to_path "$HOME/.local/bin"
# if [ -d "$HOME/.local/bin" ] ; then
#     export PATH="$HOME/.local/bin:$PATH"
# fi

# set PATH so it includes user's private bin if it exists
# Make sure this one is added last!
_add_to_path "$HOME/bin"
# if [ -d "$HOME/bin" ] ; then
#     export PATH="$HOME/bin:$PATH"
# fi

# a few barebones aliases, for muscle memory's sake
if _at_hand vim; then
    vi () {
        if [[ $# -gt 0 ]]; then
            vim "$@"
        else
            vim .
        fi
    }
fi
if _at_hand emacs; then
    em () {
        emacsclient -nw --alternate-editor=vim ${@}
    }
fi

if [ -s "$HOME/.guix-profile" ]; then
    GUIX_PROFILE="$HOME/.guix-profile"
    GUIX_LOCPATH="$GUIX_PROFILE/lib/locale"
    source "$GUIX_PROFILE/etc/profile"
fi

if [ -f "$HOME/.zprofile.local.zsh" ]; then
  source "$HOME/.zprofile.local.zsh"
fi
