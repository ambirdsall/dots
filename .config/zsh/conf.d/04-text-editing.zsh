#!/usr/bin/env zsh

export PATH=$PATH:$HOME/.emacs.d/bin/
# how on earth am I supposed to use a url bar I can't jump around with emacs shortcuts
if [ ! $IS_MAC ]; then
    _at_hand gsettings && gsettings set org.gnome.desktop.interface gtk-key-theme "Emacs"
fi

# use vim mode, but with the standard set of emacs-style keychords defined in insert mode
bindkey -v
EDITOR='qed'

# VI MODE KEYBINDINGS (ins mode)
bindkey -M viins '^a'    beginning-of-line
bindkey -M viins '^b'    backward-char
bindkey -M viins '^d'    delete-char
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
