#!/usr/bin/env zsh

if _at_hand bat; then
    export PAGER='bat --plain'
    export MANPAGER="bat -l man -p"
fi

if _at_hand doom; then
    emacsman () {
        emacsclient -nw -e "
        (let ((Man-notify-method 'bully))
          (add-transient-hook! 'quit-window-hook (delete-frame))
          (man \"$1\"))"
    }
    # alias man=emacsman
fi
