# * Let's get ready to rumble
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
# Lines configured by zsh-newuser-install
HISTFILE=~/.histfile
HISTSIZE=10000
SAVEHIST=100000
export HISTCONTROL=ignorespace
setopt autocd extendedglob notify
unsetopt beep nomatch
# End of lines configured by zsh-newuser-install
# * ensure .profile has run
if [ -z $DOTPROFILE_HAS_RUN ]; then source ~/.profile; fi

# * side effects & printed output when sourcing config

if [[ -n $TMUX ]]; then
    echo "Tmux sessions:"
    tmux list-sessions
    printf "\n"
else
    printf "No tmux sessions right now!\n\n"
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
# how on earth am I supposed to use a url bar I can't jump around with emacs shortcuts
if [ ! $IS_MAC ]; then
    gsettings set org.gnome.desktop.interface gtk-key-theme "Emacs"
fi

# C-hjkl clobbers C-k, and kill-line is not negotiable if I'm using emacs bindings. So: vim.
bindkey -v
EDITOR='qed'

# VI MODE KEYBINDINGS (ins mode)
bindkey -M viins '^a'    beginning-of-line
bindkey -M viins '^e'    end-of-line
bindkey -M viins '^r'    history-incremental-pattern-search-backward
bindkey -M viins '^s'    history-incremental-pattern-search-forward
bindkey -M viins '^p'    up-line-or-history
bindkey -M viins '^n'    down-line-or-history
bindkey -M viins '^y'    yank
bindkey -M viins '^w'    backward-kill-word
bindkey -M viins '^u'    backward-kill-line
bindkey -M viins '^h'    backward-delete-char
bindkey -M viins '^?'    backward-delete-char
bindkey -M viins '^_'    undo
bindkey -M viins '^x^r'  redisplay
bindkey -M viins '\eOH'  beginning-of-line # Home
bindkey -M viins '\eOF'  end-of-line       # End
bindkey -M viins '\e[2~' overwrite-mode    # Insert
bindkey -M viins '\ef'   forward-word      # Alt-f
bindkey -M viins '\eb'   backward-word     # Alt-b
bindkey -M viins '\ed'   kill-word         # Alt-d


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
export PAGER='bat --plain'
export MANPAGER="/bin/sh -c \"col -b | vim -c 'set ft=man ts=8 nomod nolist nonu noma' -\""

# * configure modifier keys
# repeated invocations of an xcape mapping without killing in between causes
# xcape to send multiples of the new mappings, which is terrible.
if command -v xcape; then
ps aux | grep -i '[x]cape' >/dev/null && killall -9 xcape
hash xcape 2>&1 >/dev/null && \
    xcape -e 'Control_L=Escape;Caps_Lock=Escape;Shift_L=Shift_L|9;Shift_R=Shift_R|0' >/dev/null
fi
# rebind PrtSc
if command -v xmodmap; then xmodmap ~/.Xmodmap; fi

# * Prompt
# ** util
strip_newline () {
    tr -d '\n'
}

# ** reverse dir stack (the more intuitive order IMO)
# TODO fix handling of directories with spaces in their names
reverse_dir_stack () {
    dirs | awk '{ for (i=NF; i>1; i--) printf("%s %%F{238}᎒%%f", $i) }'
}

# ** ...roll my own git status!
current_branch () {
    local BRANCH=$(git rev-parse --abbrev-ref HEAD 2>&/dev/null | strip_newline)
    if [[ $BRANCH == "HEAD" ]]; then printf "%%F{136}:"
    else
        if [[ -n $BRANCH ]]; then printf "($BRANCH) "; fi
    fi
}

current_commit () {
    git rev-parse --short HEAD 2>&/dev/null | strip_newline
}

dirty_check () {
    # TODO: number of staged files, number of modified files
    local GIT_DIRTY_CHECK_STATUS=$(git status --porcelain 2>& /dev/null)
    local GIT_DIRTY_CHECK_STATUS_COLUMNS=$(echo $GIT_DIRTY_CHECK_STATUS | awk '{print $1}')

    # display something if any tracked files have been modified
    echo $GIT_DIRTY_CHECK_STATUS_COLUMNS | grep 'M' > /dev/null && printf "%%F{160} ⁂ %%f" || true
    # echo $GIT_DIRTY_CHECK_STATUS | grep 'M' > /dev/null && printf " shit" || true

    # display something if there are untracked files
    echo $GIT_DIRTY_CHECK_STATUS_COLUMNS | grep '??' > /dev/null && printf "..." || true
}

PS1_self_implemented='$(reverse_dir_stack)%F{cyan}%~%f $(current_branch)%F{136}$(current_commit)%f$(dirty_check) $(~/bin/moon-phase)
%(?.%F{239}.%F{196})ᐇ%f '

# ** but for real
if [ $IS_MAC ]; then
    source "/usr/local/opt/zsh-git-prompt/zshrc.sh"
else
    source "/usr/lib/zsh-git-prompt/zshrc.sh"
fi

PS1='%F{239}┌ %f$(reverse_dir_stack)%F{cyan}%~%f $(git rev-parse --is-inside-work-tree &>/dev/null && git_super_status || echo -e "\b") $(~/bin/moon-phase) %F{3}$(current_commit 2>/dev/null)%f
%F{239}└%f%(?.%F{239}.%F{196})➣%f '

# * asdf
# source /opt/asdf-vm/asdf.sh
# source /opt/asdf-vm/completions/asdf.bash

# * rbenv
if [ $IS_MAC ]; then
    export RUBY_CONFIGURE_OPTS="--with-openssl-dir=$(brew --prefix openssl@1.1)"
    eval "$(rbenv init -)"
fi

# * direnv
if command -v direnv; then
    eval "$(direnv hook zsh)"
fi

# * dotfile maintenance
dots () {
    if [[ $# -gt 0 ]]; then
        git --git-dir=$HOME/.dots/ --work-tree=$HOME ${@}
    else
        git --git-dir=$HOME/.dots/ --work-tree=$HOME status
    fi
}

# * local config
[[ -a ~/.zshrc.local.zsh ]] && source ~/.zshrc.local.zsh

# * Aliases
[[ -a ~/aliases.zsh ]] && source ~/aliases.zsh
