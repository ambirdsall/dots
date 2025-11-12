# Edit the files
alias zrc="emv ~/.zshrc"
alias zrl="emv ~/.zshrc.local.zsh"
alias el="emv ~/.zshrc.local.zsh"
alias rc="source ~/.zshrc"
alias RC="VERBOSE_ZSH_CONFIG=t source ~/.zshrc"
alias et="emv ~/.tmux.conf"
ez () {
    local alias_file=$(find ~/.config/zsh/ -name '*.zsh' | fzf)
    [ -n "$alias_file" ] && emv $alias_file
}
alias ea=ez

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
