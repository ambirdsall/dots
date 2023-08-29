#!/usr/bin/env zsh

dots () {
    if [[ $# -gt 0 ]]; then
        git --git-dir=$HOME/.dots/ --work-tree=$HOME ${@}
    else
        git --git-dir=$HOME/.dots/ --work-tree=$HOME status
    fi
}
