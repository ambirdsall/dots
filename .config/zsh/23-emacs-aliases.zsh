# alias e="qed"
e() {
  emacsclient -nw --alternate-editor="" "$@"
}

edo() {
  emacsclient -nw --eval "$@"
}

E() {
  emacsclient -c --alternate-editor="" "$@" &
  disown
}

# Don't want to mess with the existing session?
alias ee="emacs -nw"
EE() {
  emacs "$@" &
  disown
}

# No idea why the fuck the current session isn't starting properly?
EEE() {
  emacsclient -c --alternate-editor="emacs --debug-init" "$@" &
  disown
}

# sometimes in life, you don't have emacs installed on a new computer yet.
# These also tend to be times when you open config files a lot! And even when
# you do have emacs installed, cracking open a config/alias/whatever file is
# a scenario where falling back to vim is preferable to a slow-booting editor.
emv() {
  if command -v emacs >/dev/null; then
    emacsclient -nw --alternate-editor="vim" $@
  else
    vim $@
  fi
}

em() {
  local file_to_edit=$(um)
  [[ -n $file_to_edit ]] && e $file_to_edit
}

rem() {
  e . -eval "(require 'projectile)(dired (directile-project-root))"
}

todos() {
  e --quiet -eval "(require 'org-projectile) (call-interactively 'org-projectile/get-todos)"
}

slay() {
  # TODO:clean up long filepaths from output
  local pid=$(ps $(pgrep Emacs) | awk 'NR>1' | fzf | awk '{print $1}')
  if [[ -n $pid ]]; then
    echo Killing process ${pid}...
    kill ${pid}
  fi
}

alias tetris='emacs -q --no-splash -f tetris'

# oh, oh, it's ~magit~
magit() {
  emacsclient --alternate-editor='' -nw -e "
(progn
  (or (advice-member-p 'save-buffers-kill-terminal '+magit/quit)
      (advice-add '+magit/quit :before 'save-buffers-kill-terminal))
  (magit-status))"
}

unicode() {
  emacsclient -nw --eval '(progn (copy-unicode-char-to-clipboard)(kill-frame))'
  echo "Copied $(pbpaste) to the clipboard"
}
