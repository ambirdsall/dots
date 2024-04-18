#!/usr/bin/env sh

# * 1 :: install keyd
if [ ! -d "~/c/keyd" ]; then
    cd ~/c
    git clone https://github.com/rvaiya/keyd
    cd keyd
    make && sudo make install
fi

# * 2 :: install config
sudo cp ~/.config/keyd/default.conf /etc/keyd/default.conf

# * 3 :: put the keyd in the ignitiond
if ! systemctl -q is-active keyd; then
    sudo systemctl enable keyd && sudo systemctl start keyd
fi
