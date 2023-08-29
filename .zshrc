for conf in ~/.config/zsh/conf.d/*.zsh; do
    source "$conf"
done

[[ -a ~/aliases.zsh ]] && source ~/aliases.zsh

# this should always go last, to allow local overrides of anything
[[ -a ~/.zshrc.local.zsh ]] && source ~/.zshrc.local.zsh
