# This file gets such a low prefix number for two reasons:
# 1) although homebrew is in nobody's POSIX spec, package management is pretty fundamental
# 2) sourcing it after language-specific tooling (nv,rbenv,pyenv,asdf,etc) causes problems

if _at_hand /opt/homebrew/bin/brew; then
    eval "$(/opt/homebrew/bin/brew shellenv)"
fi
