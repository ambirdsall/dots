#!/usr/bin/env zsh

typeset -Ag abbreviations
abbreviations=(
"A"    "| awk"
"pag"  "| agrep"
"pb"   "| bc"
"pgr"  "| groff -s -p -t -e -Tlatin1 -mandoc"
"pf"   "| fzf"
"pg"   "| rg -S"
"G"    "| rg -S"
"H"    "| head"
"J"    "| jq"
"pk"   "| keep"
"L"    "| less" # yeah, this is misleading; but less > more and pl is taken
"pt"   "| tail -f"
"W"    "| wc"
"X"    "| xargs"
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
