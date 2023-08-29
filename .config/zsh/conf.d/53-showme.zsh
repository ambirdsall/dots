#!/usr/bin/env zsh

if _at_hand bat; then
  showme () {
    bat $(which $1)
  }
else
  showme () {
    cat $(which $1)
  }
fi
