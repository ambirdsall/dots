#!/usr/bin/env zsh

list_multiplexer_sessions () {
    local zellij_sessions

    if [[ -n $TMUX ]]; then
        echo "Tmux sessions:"
        tmux list-sessions
        printf "\n"
    fi

    if dots/at_hand zellij; then
        zellij_sessions=$(zellij list-sessions 2>& /dev/null || true)

        if [ -n $zellij_sessions ]; then
            echo "Zellij sessions:"
            zellij list-sessions
            printf "\n"
        fi
    fi
}

t () {
    if ! test -n "$(ps -e | grep -q tmux)"; then
        t.up
    elif [[ $# -eq 0 ]]; then
        local session=$(tmux list-sessions | cut -d : -f 1 | fzf)
        [[ -n $session ]] && tmux attach -t $session
    else
        tmux "$@"
    fi
}
alias ta="tmux attach -t"
alias tt="tmux attach -t"
alias tk="tmux kill-session -t"

tn () {
    if [[ $# -gt 0 ]]; then
        tmux new -s $1; cd; clear
    else
        tmux new -s $(basename $(pwd)); cd; clear
    fi
}
# When the tmux session shrinks some and fills the margin with periods, it
# thinks there's another instance of the session in a smaller terminal. F that.
alias tda="tmux detach -a"
alias tls="tmux list-sessions"
