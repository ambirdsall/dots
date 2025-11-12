# Edit the files
alias zrc="emv ~/.config/zsh/config.org"
alias zrl="emv ~/.zshrc.local.zsh"
alias ez="emv ~/.config/zsh/config.org"
alias el="emv ~/.zshrc.local.zsh"
alias rc="source ~/.zshrc"
alias RC="VERBOSE_ZSH_CONFIG=t source ~/.zshrc"
alias et="emv ~/.tmux.conf"

# Reload the files
sdf () {
  local VERBOSE=t
  local grepper
  local files_with_aliases

  case $1 in
      -q|--quiet)
          shift
          unset VERBOSE
          ;;
  esac


  if command -v rg >/dev/null; then
    grepper=rg;
  else
    grepper=grep;
  fi

  files_with_aliases=($($grepper -l alias ~/.config/zsh/**/* | $grepper -v README) ~/.local-aliases.zsh)

  for file in $files_with_aliases; do
    [ -f $file ] && source $file
    [ -n "$VERBOSE" -a -f $file ] && echo sourced $file
  done

  [ -f ~/.local-aliases.zsh ] && source ~/.local-aliases.zsh || true
}

alias SDF="tmux-send-all sdf -q"
