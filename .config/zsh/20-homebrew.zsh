# This file gets such a low prefix number for two reasons:
# 1) although homebrew is in nobody's POSIX spec, package management is pretty fundamental
# 2) sourcing it after language-specific tooling (nv,rbenv,pyenv,asdf,etc) causes problems

if dots/at_hand /opt/homebrew/bin/brew; then
    eval "$(/opt/homebrew/bin/brew shellenv)"
elif dots/at_hand /home/linuxbrew/.linuxbrew/bin/brew; then
    eval "$(/home/linuxbrew/.linuxbrew/bin/brew shellenv)"
fi
