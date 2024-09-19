#!/usr/bin/env zsh

if ! [[ $(type vi) =~ 'function' ]]; then
    vi () {
        if [[ $# -gt 0 ]]; then
            vim "$@"
        else
            vim .
        fi
    }
fi
alias ci=vi
