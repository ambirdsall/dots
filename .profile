# ~/.profile: executed by the command interpreter for login shells.
# This file is not read by bash(1), if ~/.bash_profile or ~/.bash_login
# exists.
# see /usr/share/doc/bash/examples/startup-files for examples.
# the files are located in the bash-doc package.

# the default umask is set in /etc/profile; for setting the umask
# for ssh logins, install and configure the libpam-umask package.
#umask 022

export DOTPROFILE_HAS_RUN=t

# if running bash
if [ -n "$BASH_VERSION" ]; then
    # include .bashrc if it exists
    if [ -f "$HOME/.bashrc" ]; then
        . "$HOME/.bashrc"
    fi
fi

# set PATH so it includes user's private bin if it exists
if [ -d "$HOME/bin" ] ; then
    PATH="$HOME/bin:$PATH"
fi

# set PATH so it includes user's private bin if it exists
if [ -d "$HOME/.local/bin" ] ; then
    PATH="$HOME/.local/bin:$PATH"
fi

if [ -d "$HOME/.yarn/bin" ] ; then
    PATH="$HOME/.yarn/bin:$PATH"
fi

# a few barebones aliases, for muscle memory's sake
vi () {
    if [[ $# -gt 0 ]]; then
        vim "$@"
    else
        vim .
    fi
}
em () {
    emacsclient -nw --alternate-editor=emacs ${@}
}

if [ $(uname) != 'Darwin' ]; then
    local XCAPE_OPTS='Control_L=Escape;Shift_L=Shift_L|9;Shift_R=Shift_R|0'
    if [ -z $XCAPE_SET ]; then xcape -e $XCAPE_OPTS; fi
    export XCAPE_SET=t

    if [ -z $XMODMAP_SET ]; then xmodmap ~/.Xmodmap; fi
    export XMODMAP_SET=t

    export PATH="$HOME/.cargo/bin:$PATH"
    export PATH='/home/linuxbrew/.linuxbrew/bin:/home/linuxbrew/.linuxbrew/sbin':"$PATH"
    export PATH='/home/linuxbrew/.linuxbrew/bin:/home/linuxbrew/.linuxbrew/sbin':"$PATH"
fi

export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
