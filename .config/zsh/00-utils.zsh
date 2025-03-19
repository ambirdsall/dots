#!/usr/bin/env zsh

dots/add_to_path() {
    local BIN_DIR="$1"
    if [ -d "$BIN_DIR" -a ! $(echo $PATH | grep "$BIN_DIR(:|$)") ]; then
        export PATH="$BIN_DIR:$PATH"
    fi
}

dots/at_hand() {
    command -v $1 >/dev/null
}

dots/clear_name() {
    if alias | grep "$1=" >/dev/null; then
        unalias $1
    fi
}

dots/file() {
    if [ -f "$1" ]; then
        return 0
    else
        return 1
    fi
}

# Opposite of `unset`.
# TODO accept multiple args, returning 0 if *all* are set
dots/is_set() {
    if [ -n "$1" ]; then
        return 0
    else
        return 1
    fi
}

# inside a session
dots/ssh() {
    if dots/is_set "$SSH_CLIENT" || dots/is_set "$SSH_TTY"; then
        return 0
    else
        return 1
    fi
}

case $(uname) in
Darwin)
    export IS_MAC=t
    export OS_TYPE=macos
    export OS=🍎
    ;;
Linux)
    export IS_LINUX=t
    export OS_TYPE=linux
    export OS=🐧
    ;;
*)
    export OS_TYPE=wtf
    export OS=👾
    ;;
esac
