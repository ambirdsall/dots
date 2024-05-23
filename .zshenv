#!/usr/bin/env zsh

# adds the directory to $PATH *iff*:
#   1) the directory exists
#   2) the directory is not in $PATH already
# New directories are prepended, so the most recent additions take precedence. No effort
# is taken to escape extended grep metacharacters, so if your directory name can't be used
# verbatim to match itself, you're going to have to do that one the old-fashioned way.
dots/add_to_path () {
    local BIN_DIR="$1"
    if [ -d "$BIN_DIR" -a ! $(echo $PATH | grep "$BIN_DIR(:|$)") ]; then
        export PATH="$BIN_DIR:$PATH"
    fi
}

dots/at_hand () {
    command -v $1 > /dev/null
}

if dots/at_hand wezterm; then
    export TERMINAL=wezterm
elif dots/at_hand kitty; then
    export TERMINAL=kitty
elif dots/at_hand alacritty; then
    export TERMINAL=alacritty
elif dots/at_hand xfce4-terminal; then
    export TERMINAL=xfce4-terminal
fi

dots/add_to_path "$HOME/.yarn/bin"
dots/add_to_path "$HOME/.cargo/bin"
[ -s "$HOME/.cargo/env" ] && source "$HOME/.cargo/env"
dots/add_to_path "$HOME/.local/bin"
# Make sure this one is added last!
dots/add_to_path "$HOME/bin"

# a few barebones aliases, for muscle memory's sake
if dots/at_hand vim; then
    vi () {
        if [[ $# -gt 0 ]]; then
            vim "$@"
        else
            vim .
        fi
    }
fi
if dots/at_hand emacs; then
    em () {
        emacsclient -nw --alternate-editor=vim ${@}
    }
fi

if [[ $XDG_SESSION_TYPE -eq wayland ]]; then
    export MOZ_ENABLE_WAYLAND=1
fi

if [ -s "$HOME/.guix-profile" ]; then
    GUIX_PROFILE="$HOME/.guix-profile"
    GUIX_LOCPATH="$GUIX_PROFILE/lib/locale"
    source "$GUIX_PROFILE/etc/profile"
fi

if [ -f "$HOME/.zshenv.local.zsh" ]; then
  source "$HOME/.zshenv.local.zsh"
fi
