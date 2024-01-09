# heh
for file in $(ls ~/.config/zsh/conf.d/*-aliases.zsh); do
    source $file;
done

[ -f ~/.local-aliases.zsh ] && source ~/.local-aliases.zsh || true
