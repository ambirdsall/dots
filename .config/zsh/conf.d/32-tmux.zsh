#!/usr/bin/env zsh

if [[ -n $TMUX ]]; then
    echo "Tmux sessions:"
    tmux list-sessions
    printf "\n"
fi

alias clear='clear; [[ -z "$TMUX" ]] && tls 2>/dev/null || true'

alias t=tmux
alias ta="tmux attach -t"
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
