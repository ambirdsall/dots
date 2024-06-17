#!/usr/bin/env zsh

[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh


if dots/at_hand fzf; then
  alias um="find . -type f | grep -vE '.tmp-*|.git|node_modules|bower_components' | fzf --multi --preview 'bat --color=always {}'"
  cdf() {
    pushd $(dirname $(find . -type f | grep -vE '.tmp-*|.git|node_modules|bower_components|DS_Store' | fzf --preview 'bat --color=always {}'))
  }

  if dots/at_hand fzf-tmux; then
    alias fzf=fzf-tmux
  fi
fi
