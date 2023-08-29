#!/usr/bin/env zsh

# it's just a better clipboard API, tbh; plus muscle memory
if [ $(uname) != 'Darwin' ]; then
  if command -v wl-copy &>/dev/null; then
    alias pbcopy='wl-copy'
    alias pbpaste='wl-paste --type text'
  elif command -v xclip &>/dev/null; then
    alias pbcopy='xclip -i -selection clipboard'
    alias pbpaste='xclip -o -selection clipboard'
  fi
fi
