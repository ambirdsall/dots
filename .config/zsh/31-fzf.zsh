[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

if dots/at_hand fzf; then
    alias um="find . -type f | grep -vE '\.tmp-*|\.git|node_modules|bower_components' | fzf --multi --preview 'bat --color=always {}'"
    cdf() {
        pushd $(dirname $(find . -type f | grep -vE '.tmp-*|.git|node_modules|bower_components|DS_Store' | fzf --preview 'bat --color=always {}'))
    }

    if dots/at_hand fzf-tmux; then
        alias fzf=fzf-tmux
    fi

    source <(fzf --zsh)

    __fzf_select_files() {
        local selected
        selected=$(find . -type f \
            | grep -vE '\.tmp-*|\.git|node_modules|bower_components' \
            | fzf --multi --preview 'bat --color=always {}' \
            | while IFS= read -r item; do printf '%q ' "$item"; done)

        # Insert the selected file(s) at the cursor position
        LBUFFER+="${selected% }"  # trim trailing space
        zle redisplay
    }

    zle -N __fzf_select_files

    # the mnemonic is "select"
    bindkey '^S' __fzf_select_files
fi
