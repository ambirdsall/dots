# TODO: move to ~/.config/zsh/conf.d/52-edit-config-file-aliases.zsh
main () {
  local VERBOSE
  local grepper
  local files_with_aliases

  if [ "$1" == --verbose ]; then
    VERBOSE=t
  fi

  if command -v rg >/dev/null; then
    grepper=rg;
  else
    grepper=grep;
  fi

  files_with_aliases=($($grepper -l alias ~/.config/zsh/conf.d/* | $grepper -v README) ~/.local-aliases.zsh)

  for file in $files_with_aliases; do
    [ -f $file ] && source $file
    [ -n "$VERBOSE" -a -f $file ] && echo sourced $file
  done
}

main "$@"

# [ -f ~/.local-aliases.zsh ] && source ~/.local-aliases.zsh || true
