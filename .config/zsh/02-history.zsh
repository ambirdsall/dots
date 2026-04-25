HISTFILE=~/.histfile
HISTSIZE=10000
SAVEHIST=10000
HISTCONTROL=ignorespace

setopt SHARE_HISTORY
setopt HIST_IGNORE_DUPS
setopt HIST_IGNORE_ALL_DUPS

if dots/at_hand fzf; then
  r () {
    $(fc -l -n 1 | tail -r | fzf)
  }
fi
