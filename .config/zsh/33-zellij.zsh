#!/usr/bin/env zsh

# So. These are yoinked from the output of running zellij's zsh autocomplete++ command
# (`zellij setup --generate-completion zsh`); although I do rather want that autocomplete
# functionality, it is currently throwing syntax errors. So:
# TODO regenerate the autocompletion definitions after updating zellij
if dots/at_hand zellij; then
  function zr () { zellij run --name "$*" -- zsh -ic "$*";}
  function zrf () { zellij run --name "$*" --floating -- zsh -ic "$*";}
  function zri () { zellij run --name "$*" --in-place -- zsh -ic "$*";}
  function ze () { zellij edit "$*";}
  function zef () { zellij edit --floating "$*";}
  function zei () { zellij edit --in-place "$*";}
  function zpipe () {
  if [ -z "$1" ]; then
    zellij pipe;
  else
    zellij pipe -p $1;
  fi
  }
fi
