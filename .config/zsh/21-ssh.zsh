# make it work on macOS, too
if [[ $OS_TYPE == "macos" ]]; then
    # never bunt
    export XDG_RUNTIME_DIR=$HOME/.run
    mkdir -p $XDG_RUNTIME_DIR
fi

# get ssh-agent running all the dang time
if ! pgrep -u "$USER" ssh-agent > /dev/null; then
    ssh-agent > "$XDG_RUNTIME_DIR/ssh-agent.env"
fi
if [[ ! -f "$SSH_AUTH_SOCK" ]]; then
    [[ -f "$XDG_RUNTIME_DIR/ssh-agent.env" ]] && source "$XDG_RUNTIME_DIR/ssh-agent.env" >/dev/null
fi

# TODO automatically run `ssh-add` if and only if it hasn't yet been run this session
# i.e. only enter my ssh key passphrase once, and that only on demand
# possibly via an ssh wrapper script?

# fun output when logging into a computer of mine
if [[ -n $SSH_CONNECTION ]]; then
    if command -v neofetch &>/dev/null; then
        if command -v lolcat &>/dev/null; then
            neofetch | lolcat -S 10 -F 0.05
        else
            neofetch
        fi
    fi
fi
