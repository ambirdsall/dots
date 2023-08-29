#!/usr/bin/env zsh

_add_to_path () {
    local BIN_DIR="$1"
    if [ -d "$BIN_DIR" -a ! $(echo $PATH | grep "$BIN_DIR(:|$)") ]; then
        export PATH="$BIN_DIR:$PATH"
    fi
}

_at_hand () {
    command -v $1 > /dev/null
}

if [[ $(uname) == 'Darwin' ]]; then
    export IS_MAC=t
fi
