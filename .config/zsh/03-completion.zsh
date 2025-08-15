#!/usr/bin/env zsh

zstyle ':completion:*' completer _expand _complete _ignored

autoload -Uz compinit && compinit

# load personal completion scripts
for file in $(ls ~/lib/zsh/*-comp.zsh); do
    source $file
done
