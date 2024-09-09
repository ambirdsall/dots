#!/usr/bin/env zsh

# alias e="qed"
e () {
  emacsclient -nw --socket-name=ttylated --alternate-editor="" "$@"
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
    emacsclient -nw --socket-name=ttylated --alternate-editor="vim" $@
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

# oh, oh, it's ~magit~
magit () {
    emacsclient --socket=magit -nw -e "
(progn
  (or (advice-member-p 'save-buffers-kill-terminal '+magit/quit)
      (advice-add '+magit/quit :before 'save-buffers-kill-terminal))
  (magit-status))" || emacs --daemon=magit
}

man () {
  emacsclient -nw --socket-name=ttylated --eval "
(progn
  (man \"$@\")
  (or
    (functionp 'amb/man-clean-up-after-yourself)
    (defun amb/man-clean-up-after-yourself ()
      (remove-hook 'kill-buffer-hook 'amb/man-clean-up-after-yourself)
      (delete-frame)))
  (if (string-prefix-p \"*Man\" (buffer-name))
    (doom/window-maximize-buffer)
   (run-with-timer 0.3 nil (fn! (doom/window-maximize-buffer))))
  (add-hook 'kill-buffer-hook #'amb/man-clean-up-after-yourself))"
}

unicode () {
   emacsclient -nw --socket-name=ttylated --eval '(progn (copy-unicode-char-to-clipboard)(kill-frame))'
   echo "Copied $(pbpaste) to the clipboard"
}
