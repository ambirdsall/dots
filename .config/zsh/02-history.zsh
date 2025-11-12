export HISTFILE=~/.histfile
export HISTSIZE=10000
export SAVEHIST=100000
export HISTCONTROL=ignorespace

if dots/at_hand fzf; then
  r () {
    $(fc -l -n 1 | tail -r | fzf)
  }
fi
