#!/usr/bin/env zsh

alias ea="emv ~/aliases.zsh"
eaa () {
    local alias_file=$(find ~/.config/zsh/conf.d/ -name '*-aliases.zsh' | fzf)
    [ -n "$alias_file" ] && emv $alias_file
}
alias el="emv ~/.local-aliases.zsh"
sdf () {
    source ~/aliases.zsh --verbose
    true
}
alias SDF="tmux-send-all sdf"
alias zrc="emv ~/.zshrc"
alias zrl="emv ~/.zshrc.local.zsh"
alias rc="source ~/.zshrc"
alias et="emv ~/.tmux.conf"
