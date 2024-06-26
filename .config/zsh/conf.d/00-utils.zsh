#!/usr/bin/env zsh

dots/add_to_path () {
    local BIN_DIR="$1"
    if [ -d "$BIN_DIR" -a ! $(echo $PATH | grep "$BIN_DIR(:|$)") ]; then
        export PATH="$BIN_DIR:$PATH"
    fi
}

dots/at_hand () {
    command -v $1 > /dev/null
}

dots/clear_name () {
    if alias | grep "$1=" > /dev/null; then
        unalias $1
    fi
}

if [[ $(uname) == 'Darwin' ]]; then
    export IS_MAC=t
fi
