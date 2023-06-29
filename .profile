# ~/.profile: executed by the command interpreter for login shells.
# This file is not read by bash(1), if ~/.bash_profile or ~/.bash_login
# exists.
# see /usr/share/doc/bash/examples/startup-files for examples.
# the files are located in the bash-doc package.

# the default umask is set in /etc/profile; for setting the umask
# for ssh logins, install and configure the libpam-umask package.
#umask 022

# if running bash
if [ -n "$BASH_VERSION" ]; then
    # include .bashrc if it exists
    if [ -f "$HOME/.bashrc" ]; then
        . "$HOME/.bashrc"
    fi
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

# TODO: make in some way conditional on sway/wayland actually running
export MOZ_ENABLE_WAYLAND=1
export XDG_CURRENT_DESKTOP=sway
export XDG_SESSION_TYPE=wayland
export WLR_NO_HARDWARE_CURSORS=1
#export WLR_NO_HARDWARE_CURSORS=0
export WLR_RENDERER_ALLOW_SOFTWARE=1
