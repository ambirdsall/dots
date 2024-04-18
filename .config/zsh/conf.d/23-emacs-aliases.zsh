#!/usr/bin/env zsh

# alias e="qed"
e () {
  emacsclient -nw --socket-name=tty --alternate-editor="" "$@"
}

E () {
  emacsclient -c --socket-name=interminable --alternate-editor="" "$@" & disown
}

# Don't want to mess with the existing session?
alias ee="emacs -nw"
EE () {
  emacs "$@" & disown
}

# No idea why the fuck the current session isn't starting properly?
EEE () {
  emacsclient -c --socket-name=interminable --alternate-editor="emacs --daemon=interminable --debug-init" "$@" & disown
}

# sometimes in life, you don't have emacs installed on a new computer yet.
# These also tend to be times when you open config files a lot! And even when
# you do have emacs installed, cracking open a config/alias/whatever file is
# a scenario where falling back to vim is preferable to a slow-booting editor.
emv () {
  if command -v emacs > /dev/null; then
    emacsclient -nw --socket-name=tty --alternate-editor="vim" $@
  else
    vim $@
  fi
}

emm () {
  e $(um)
}

rem () {
  e . -eval "(require 'projectile)(dired (directile-project-root))"
}

todos () {
  e --quiet -eval "(require 'org-projectile) (call-interactively 'org-projectile/get-todos)"
}

slay () {
    # TODO:clean up long filepaths from output
    local pid=$(ps $(pgrep Emacs) | awk 'NR>1' | fzf | awk '{print $1}')
    if [[ -n $pid ]]; then
        echo Killing process ${pid}...
        kill ${pid}
    fi
}

alias tetris='emacs -q --no-splash -f tetris'

unicode () {
   emacsclient -nw --socket-name=tty --eval '(progn (copy-unicode-char-to-clipboard)(kill-frame))'
   echo "Copied $(pbpaste) to the clipboard"
}
