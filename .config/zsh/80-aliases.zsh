alias fuck="rm -rf"
if dots/at_hand xdg-open; then
    if ! dots/at_hand open; then
        alias open=xdg-open
    fi
fi

# This should always be last, so anything above can be overwritten
[ -f ~/.local-aliases.zsh ] && source ~/.local-aliases.zsh || true
