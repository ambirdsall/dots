# * Let's get ready to rumble
_add_to_path () {
    local BIN_DIR="$1"
    if [ -d "$BIN_DIR" -a ! $(echo $PATH | grep "$BIN_DIR(:|$)") ]; then
        export PATH="$BIN_DIR:$PATH"
    fi
}

_at_hand () {
    command -v $1 > /dev/null
}

if [[ $(uname) == 'Darwin' ]]; then
    export IS_MAC=t
fi

# * Configure macOS options
# per some quick trial and error, this is the largest value the system will accept
if [ $IS_MAC ]; then
    ulimit -n 24576
else
    # TODO: check and set reasonable value for linux
fi

# * Configure zsh features
HISTFILE=~/.histfile
HISTSIZE=10000
SAVEHIST=100000
export HISTCONTROL=ignorespace
setopt autocd extendedglob notify
unsetopt beep nomatch

# * side effects & printed output when sourcing config

if [[ -n $TMUX ]]; then
    echo "Tmux sessions:"
    tmux list-sessions
    printf "\n"
fi

if [[ -f /usr/share/calendar/calendar.history ]]; then
    echo "Today in history:"
    grep $(date +'%m/%d') < /usr/share/calendar/calendar.history | shuf | head -n 1
    printf "\n"
fi

&>/dev/null rm ~/Desktop/Screenshot*.png

# f that c-s
stty -ixon

# * Completion setup
# The following lines were added by compinstall
# zstyle :compinstall filename '/home/amb/.zshrc'
zstyle ':completion:*' completer _expand _complete _ignored

autoload -Uz compinit
compinit

_grepish() {
    if (( CURRENT == 2 )); then
        compadd $(cut -f 1 .git/tags tmp/tags 2>/dev/null | grep -v '!_TAG')
    fi
}
compdef _grepish ag rg ack grep

# allow mv actions on multiple files, see:
autoload -U zmv
setopt auto_cd PROMPT_SUBST

# * Text editing
export PATH=$PATH:$HOME/.emacs.d/bin/
# how on earth am I supposed to use a url bar I can't jump around with emacs shortcuts
if [ ! $IS_MAC ]; then
    gsettings set org.gnome.desktop.interface gtk-key-theme "Emacs"
fi

# C-hjkl clobbers C-k, and kill-line is not negotiable if I'm using emacs bindings. So: vim.
bindkey -v
EDITOR='qed'

# VI MODE KEYBINDINGS (ins mode)
bindkey -M viins '^a'    beginning-of-line
bindkey -M viins '^b'    backward-char
bindkey -M viins '^e'    end-of-line
bindkey -M viins '^f'    forward-char
bindkey -M viins '^h'    backward-delete-char
bindkey -M viins '^k'    kill-line
bindkey -M viins '^n'    down-line-or-history
bindkey -M viins '^p'    up-line-or-history
bindkey -M viins '^r'    history-incremental-pattern-search-backward
bindkey -M viins '^s'    history-incremental-pattern-search-forward
bindkey -M viins '^t'    transpose-chars
bindkey -M viins '^u'    backward-kill-line
bindkey -M viins '^w'    backward-kill-word
bindkey -M viins '^y'    yank
bindkey -M viins '^?'    backward-delete-char
bindkey -M viins '^_'    undo
bindkey -M viins '^x^r'  redisplay
bindkey -M viins '\eOH'  beginning-of-line # Home
bindkey -M viins '\eOF'  end-of-line       # End
bindkey -M viins '\e[2~' overwrite-mode    # Insert
bindkey -M viins '\ef'   forward-word      # Alt-f
bindkey -M viins '\eb'   backward-word     # Alt-b
bindkey -M viins '\ed'   kill-word         # Alt-d
bindkey -M viins '\et'   transpose-words


# VI MODE KEYBINDINGS (cmd mode)
bindkey -M vicmd '^a'    beginning-of-line
bindkey -M vicmd '^e'    end-of-line
bindkey -M vicmd '^r'    history-incremental-pattern-search-backward
bindkey -M vicmd '^s'    history-incremental-pattern-search-forward
bindkey -M vicmd '^p'    up-line-or-history
bindkey -M vicmd '^n'    down-line-or-history
bindkey -M vicmd '^y'    yank
bindkey -M vicmd '^w'    backward-kill-word
bindkey -M vicmd '^u'    backward-kill-line
bindkey -M vicmd '/'     vi-history-search-forward
bindkey -M vicmd '?'     vi-history-search-backward
bindkey -M vicmd '^_'    undo
bindkey -M vicmd '\ef'   forward-word                      # Alt-f
bindkey -M vicmd '\eb'   backward-word                     # Alt-b
bindkey -M vicmd '\ed'   kill-word                         # Alt-d
bindkey -M vicmd '\e[5~' history-beginning-search-backward # PageUp
bindkey -M vicmd '\e[6~' history-beginning-search-forward  # PageDown

function zle-line-init zle-keymap-select {
    NORMAL_MODE_PROMPT="%{$fg_bold[yellow]%} [% NORMAL]%  %{$reset_color%}"
    RPS1="${${KEYMAP/vicmd/$NORMAL_MODE_PROMPT}/(main|viins)/} $EPS1"
    zle reset-prompt
}

zle -N zle-line-init
zle -N zle-keymap-select

# * PAGERs
if _at_hand bat; then
    export PAGER='bat --plain'
    export MANPAGER="bat -l man -p'"
fi

if _at_hand doom; then
    emacsman () {
        emacsclient -nw -e "
        (let ((Man-notify-method 'bully))
          (add-transient-hook! 'quit-window-hook (delete-frame))
          (man \"$1\"))"
    }
    # alias man=emacsman
fi

# * asdf
# source /opt/asdf-vm/asdf.sh
# source /opt/asdf-vm/completions/asdf.bash

# * rbenv
if command -v rbenv; then
    [ $IS_MAC ] && export RUBY_CONFIGURE_OPTS="--with-openssl-dir=$(brew --prefix openssl@1.1)"
    eval "$(rbenv init -)"
fi

# * direnv
if command -v direnv &>/dev/null; then
    eval "$(direnv hook zsh)"
fi

# * ssh
# ** get ssh-agent running all the dang time
if ! pgrep -u "$USER" ssh-agent > /dev/null; then
    ssh-agent > "$XDG_RUNTIME_DIR/ssh-agent.env"
fi
if [[ ! -f "$SSH_AUTH_SOCK" ]]; then
    [[ -f "$XDG_RUNTIME_DIR/ssh-agent.env" ]] && source "$XDG_RUNTIME_DIR/ssh-agent.env" >/dev/null
fi

# TODO automatically run `ssh-add` if and only if it hasn't yet been run this session
# i.e. only enter my ssh key passphrase once, and that only on demand
# possibly via an ssh wrapper script?

# ** fun output when logging into a computer of mine
if [[ -n $SSH_CONNECTION ]]; then
    if command -v neofetch &>/dev/null; then
        if command -v lolcat &>/dev/null; then
            neofetch | lolcat -S 10 -F 0.05
        else
            neofetch
        fi
    fi
fi

# * dotfile maintenance
dots () {
    if [[ $# -gt 0 ]]; then
        git --git-dir=$HOME/.dots/ --work-tree=$HOME ${@}
    else
        git --git-dir=$HOME/.dots/ --work-tree=$HOME status
    fi
}

# * Aliases
[[ -a ~/aliases.zsh ]] && source ~/aliases.zsh

[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

# * Source modular config files
for conf in ~/.config/zsh/conf.d/*.zsh; do
    source "$conf"
done

# * Prompt
# TODO fix handling of directories with spaces in their names
reverse_dir_stack () {
    dirs | awk '{ for (i=NF; i>1; i--) printf("%s %%F{238}᎒%%f", $i) }'
}

PROMPT='%F{239}┌ %f$(reverse_dir_stack)%F{cyan}%~%f $(git rev-parse --is-inside-work-tree &>/dev/null && gitprompt || echo -e "\b ")$(~/bin/moon-phase)
%F{239}└%f%(?.%F{239}.%F{196})➣%f '

# * local config
# this should always go last, to allow local overrides of anything
[[ -a ~/.zshrc.local.zsh ]] && source ~/.zshrc.local.zsh
