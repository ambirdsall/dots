#!/usr/bin/env zsh

typeset -Ag abbreviations
abbreviations=(
"A"     "| awk '{print $2 }'"
"B"     "| bc"
"GROFF" "| groff -s -p -t -e -Tlatin1 -mandoc"
"F"     "| fzf"
"G"     "| rg -S"
"H"     "| head"
"J"     "| jq"
"L"     "| less"
"T"     "| tail -f"
"W"     "| wc -l"
"X"     "| xargs"
)

magic-abbrev-expand () {
  local MATCH
  LBUFFER=${LBUFFER%%(#m)[_a-zA-Z0-9]#}
  LBUFFER+=${abbreviations[$MATCH]:-$MATCH}
  zle self-insert
}

no-magic-abbrev-expand () {
  LBUFFER+=' '
}

zle -N magic-abbrev-expand
zle -N no-magic-abbrev-expand
bindkey " " magic-abbrev-expand
bindkey "^x " no-magic-abbrev-expand
bindkey -M isearch " " self-insert
