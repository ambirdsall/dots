#!/usr/bin/env sh

if [ ! -d "$HOME/c/janet" ]; then
    cd ~/c
    git clone https://github.com/janet-lang/janet
    cd janet || exit 1
    make -j

    # make test

    sudo make install
    sudo make install-jpm-git
    sudo make install-spork-git

    sudo jpm install sh
    sudo jpm install cmd
fi
