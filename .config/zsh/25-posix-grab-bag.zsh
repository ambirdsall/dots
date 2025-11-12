alias awkcsv='awk -F "\"*,\"*"'

alias cx='chmod +x'

alias ls="ls -GF"
alias la="ls -A"
alias ll="ls -lh"

if dots/at_hand sl; then
    # allow exiting with C-c
    alias sl="sl -e"
fi

alias mkdir="mkdir -pv"

alias please=sudo
