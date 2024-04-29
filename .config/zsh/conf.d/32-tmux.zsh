#!/usr/bin/env zsh

list_sessions_if_inside_tmux() {
    if [[ -n $TMUX ]]; then
        echo "Tmux sessions:"
        tmux list-sessions
        printf "\n"
    fi
}

alias clear='clear; [[ -z "$TMUX" ]] && tls 2>/dev/null || true'

if command -v fzf-tmux >/dev/null; then
    alias t=fzf-tmux
    alias tt="fzf-tmux attach -t"
    alias tk="fzf-tmux kill-session -t"
else
    alias t=tmux
    alias ta="tmux attach -t"
    alias tt="tmux attach -t"
    alias tk="tmux kill-session -t"
fi

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
