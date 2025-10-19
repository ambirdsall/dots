#!/usr/bin/env sh

if [ ! -d "$HOME/c/janet" ]; then
    cd ~/c || exit 1
    git clone https://github.com/janet-lang/janet
fi
cd "$HOME/c/janet" || exit 1
make -j

# make test

# TODO switch to non-root janet install
sudo make install
sudo make install-jpm-git
sudo make install-spork-git

sudo jpm install sh
