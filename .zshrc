for conf in ~/.config/zsh/conf.d/*.zsh; do
    source "$conf"
done

list_sessions_if_inside_tmux

# TODO because most aliases got moved into ~/.config/zsh/conf.d/*, with
# ~/aliases.zsh serving as easy entry point for re-sourcing, most of these
# are getting double-sourced (with the exception of ~/.local-aliases.zsh)
[[ -a ~/aliases.zsh ]] && source ~/aliases.zsh

# this should always go last, to allow local overrides of anything
[[ -a ~/.zshrc.local.zsh ]] && source ~/.zshrc.local.zsh
