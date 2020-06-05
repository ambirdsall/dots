# * emacsclients

if ! command -v em > /dev/null; then
    tmux-session-name () {
        local tty=$(tty)
        for s in $(tmux list-sessions -F '#{session_name}' 2>/dev/null); do
            tmux list-panes -F '#{pane_tty} #{session_name}' -t "$s"
        done | grep "$tty" | awk '{print $2}'
    }

    emacsclient-context () {
        local tmux_session_name=$(tmux-session-name)
        echo ${tmux_session_name:-cli}
    }

    em () {
        emacsclient --alternate-editor="" --socket-name=${emacsclient-context} --tty --quiet ${@}
    }

    # sometimes in life, you don't have emacs installed on a new computer yet.
    # These also tend to be times when you open config files a lot!
    emv () {
        emacsclient --alternate-editor=vim --socket-name=${emacsclient-context} --tty --quiet ${@}
    }

    emm () {
        em $(um)
    }

    rem () {
        em . -eval "(require 'projectile)(dired (directile-project-root))"
    }
fi

slay () {
    # TODO:clean up long filepaths from output
    local pid=$(ps $(pgrep Emacs) | awk 'NR>1' | fzf | awk '{print $1}')
    if [[ -n $pid ]]; then
        echo Killing process ${pid}...
        kill ${pid}
    fi
}

# * pb{copy,paste}
# it's just a better clipboard API, tbh; plus muscle memory
if [ $(uname) != 'Darwin' ]; then
    alias pbcopy='xclip -i'
    alias pbpaste='xclip -o'
fi


# * cd to within code directory from anywhere
cdc () {
  if [[ $# -gt 0 ]]; then
    cdd ~/c/${@}
  else
    cdd ~/c
  fi
}
compdef '_files -W ~/c' cdc
tnc () {
  cdd ~/c/${@} && tn
}
compdef '_files -W ~/c' tnc

# * quick edit/source shell config files
alias ea="emv ~/aliases.zsh"
alias el="emv ~/.local-aliases.zsh"
alias sdf="source ~/aliases.zsh && echo sourced ~/aliases.zsh && [[ -f ~/.local-aliases.zsh ]] && source ~/.local-aliases.zsh && echo sourced ~/.local-aliases.zsh || true"
alias zrc="emv ~/.zshrc"
alias rc="source ~/.zshrc"
alias et="emv ~/.tmux.conf"

# * serve local files
alias serve="python -m SimpleHTTPServer"

# * unfuck yourself
alias sane='stty sane'

# * first you have to be able to get where you're going
# ** jump around
# DEPENDENCY: gem install rouge
alias um="find . -type f | grep -vE '.tmp-*|.git|node_modules|bower_components' | fzf --multi --preview 'bat --color=always {}'"
cdf() {
  pushd $(dirname $(find . -type f | grep -vE '.tmp-*|.git|node_modules|bower_components|DS_Store' | fzf --preview 'rougify {}'))
}
# h () {
#   local cols sep
#   cols=$(( COLUMNS / 3 ))
#   sep='{::}'

# TODO: find chrome history's location on linux
#   cp -f ~/Library/Application\ Support/Google/Chrome/Default/History /tmp/h

#   sqlite3 -separator $sep /tmp/h \
#     "select substr(title, 1, $cols), url
#      from urls order by last_visit_time desc" |
#   awk -F $sep '{printf "%-'$cols's  \x1b[36m%s\x1b[m\n", $1, $2}' |
#   fzf --ansi --multi | sed 's#.*\(https*://\)#\1#' | xargs open
# }



# * shortcuts for standard commands and builtins
# ** awk
alias awkcsv='awk -F "\"*,\"*"'

# ** background jobs & processes
# these play nicely with fg (especially with vim/emacsclient)
alias j=jobs
# Typing the percent sign gets annoying fast when you run `kill` all the time with `%n`-style arguments on suspended `jobs`
k () {
    kill %"$1"
}

# ** `cd` by any name would smell as sweet
alias 'cd-'="cd -"
alias ..="cdd .."
alias ...="cdd ../.."
alias ....="cdd ../../.."
alias .....="cdd ../../../.."
alias ......="cdd ../../../../.."
alias .......="cdd ../../../../../.."
alias ........="cdd ../../../../../../.."
alias .........="cdd ../../../../../../../.."

mcd () {
    mkdir $1
    cd $1
}

cdd () {
    cd $1
    ls -GF
}



# ** chmod
alias cx='chmod +x'

# ** clear
alias clear='clear; [[ -z "$TMUX" ]] && tls 2>/dev/null || true'

# ** echo
alias e=echo

# ** `=` :: less
= () {
    # iff there are 0 arguments given, assume input from stdin (i.e. a pipe)
    if [[ $# -gt 0 ]]; then
        /usr/share/vim/vim80/macros/less.sh "$*"
    else
        /usr/share/vim/vim80/macros/less.sh "$*" -
    fi
}

# ** `ls`
alias ls="ls -GF"
alias la="ls -A"
alias ll="ls -l"

# ** `mkdir`
alias mkdir="mkdir -pv"

# ** `rm`
alias fuck="rm -rf"

# ** `tail`
tailfh () {
  tail -f $1 | ack -i 'error' --passthru
}
alias tsslog='tail -f /tmp/tss.log'

# ** `mv`
alias mmv='noglob zmv -W'

# ** Vim and fam

if ! [[ $(type vi) =~ 'function' ]]; then
    vi () {
        if [[ $# -gt 0 ]]; then
            vim "$@"
        else
            vim .
        fi
    }
fi
alias ci=vi
bo () {
    emv $(bundle show "$1")
}

# * shortcuts for nonstandard commands
# ** tmux
alias t=tmux
alias tt="tmux attach -t"
alias tk="tmux kill-session -t"
tn () {
    if [[ $# -gt 0 ]]; then
        tmux new -s $1; cd; clear
    else
        tmux new -s $(basename $(pwd)); cd; clear
    fi
}
# When the tmux session shrinks some and fills the margin with periods, it
# thinks there's another instance of the session in a smaller terminal. F that.
alias tda="tmux detach -a"
alias tls="tmux list-sessions"

# ** `tree`
alias tree='tree -I node_modules'
# * language- or tool-specific shortcuts
# ** ruby
alias be='bundle exec'
# ** js/ts
alias scripts='jq .scripts < package.json'
# * Git gets its own top-level section
# No arguments: `git status`
# With arguments: acts like `git`
g () {
  if [[ $# -gt 0 ]]; then
    git "$@"
  else
    git status -s
  fi
}

gs () {
    emacsclient -a "" --socket=magit -nw -e "(progn (or (advice-member-p 'spacemacs/frame-killer 'magit-mode-bury-buffer) (advice-add 'magit-mode-bury-buffer :before 'spacemacs/frame-killer)) (magit-status))"
}
# Complete g like git, which is actually `hub`.
# compdef g=hub

# run `git clone` and `cdd` into dir
# if no arguments are provided, assumes you have copied a repo url to your clipboard
gc () {
  if [[ $# -gt 0 ]]; then
    repo=$(pbpaste)
  else
    repo=$1
  fi

  repo_dir_with_trailing_git=${repo##*/}
  repo_dir=${repo_dir_with_trailing_git%.git}
  git clone $repo
  cdd $repo_dir
}


co () {
  if [[ $# -gt 0 ]]; then
    git checkout "$@"
  else
    git checkout `git branch -l | sed 's/^* //' | fzf --preview 'git show heads/{} | diff-so-fancy'`
  fi
}
alias co-="git co -"

cor () {
    local BRANCH_OR_REF=$(git recent | fzf)
    if [ ! -z $BRANCH_OR_REF ]; then
        git checkout $BRANCH_OR_REF
    fi
}

cob () {
  co -b "`echo $* | tr ' ' -`"
}

d () {
  # git diff --word-diff "$@"
    git diff --diff-algorithm=minimal --color "$@" | diff-so-fancy | bat
}
alias gdc="d --cached"
alias gdo="git diff \$(git rev-parse --abbrev-ref HEAD 2> /dev/null)..origin/\$(git rev-parse --abbrev-ref HEAD 2> /dev/null)"

alias ts="tig status"

alias p="git add -p"

c () {
  if [[ $# -gt 0 ]]; then
    git commit -m "$*"
  else
    git commit -v
  fi
}
alias a="git commit --amend"
alias arh="git commit --amend --reuse-message=HEAD"

alias f="git fetch"

alias gr="git rebase"
alias gr-="git rebase -"
alias gri="g ri" # home-cooked git-ri, which simplifies syntax of `git rebase -i`

alias gp="git pull --ff-only"
alias gpr="git pull --rebase"

alias gb="git branch"
alias gbl="git branch -l"

l () {
  # no args
  if [[ $# -eq 0 ]]; then
    git log --oneline --decorate --graph --all
  # one arg which is an integer
  elif [[ $# -eq 1 ]] && [[ "$1" = <-> ]]; then
    git log --oneline --decorate -$1
  else
    echo "Usage: l [number of commits]" >&2
  fi
}
eval "$(ruby -e '9.times do |i| puts %Q{alias l#{i+1}=l\\ -#{i+1}} end')"
alias lg="git log --oneline --decorate --graph --all"
alias rl="git reflog"

unique () {
  perl -ne '$H{$_}++ or print'
}
git-recent () {
  # lists unique git refs you have checked out, in order of how recently you checked them out
  git reflog | ack --nocolor checkout: | awk '{print $6}' | unique | more
}

alias gg="git grep"

alias b="git blame"

alias stash="git stash save -u"
alias pop="git stash pop"

alias shipit='echo "       _~\n    _~ )_)_~\n    )_))_))_)\n    _!__!__!_\n    \______t/\n  ~~~~~~~~~~~~~" && git push origin $(git rev-parse --abbrev-ref HEAD 2> /dev/null)'
alias SHIPIT='echo "       _~\n    _~ )_)_~\n    )_))_))_)\n    _!__!__!_\n    \______t/\n  ~~~~~~~~~~~~~" && git push --force-with-lease origin $(git rev-parse --abbrev-ref HEAD 2> /dev/null)'
# an important part of being a good programmer is making fun of yourself when you make a typo
alias SHIIT='echo "       _~\n    _~ )_)_~\n    )_))_))_)\n    _!__!__!_\n    \______t/\n  ~~~~~~~~~~~~~" && echo "      FUUCK"'

# * Self-expanding shell abbreviations
# cf. http://zshwiki.org/home/examples/zleiab
typeset -Ag abbreviations
abbreviations=(
"pa"    "| rg"
"pag"   "| agrep"
"pb"    "| bc"
"peg"   "| egrep"
"pgr"   "| groff -s -p -t -e -Tlatin1 -mandoc"
"pf"    "| fzf"
"pg"    "| rg"
"ph"    "| head"
"pj"    "| jq"
"pk"    "| keep"
"pm"    "| less" # yeah, this is misleading; but less > more and pl is taken
"pt"    "| tail -f"
"pv"    "| vim"
"pw"    "| wc"
"px"    "| xargs"
)

magic-abbrev-expand () {
  local MATCH
  LBUFFER=${LBUFFER%%(#m)[_a-zA-Z0-9]#}
  LBUFFER+=${abbreviations[$MATCH]:-$MATCH}
  zle self-insert
}

no-magic-abbrev-expand () {
  LBUFFER+=' '
}

zle -N magic-abbrev-expand
zle -N no-magic-abbrev-expand
bindkey " " magic-abbrev-expand
bindkey "^x " no-magic-abbrev-expand
bindkey -M isearch " " self-insert


# * Unicode arts and farts
alias idk="echo -n '¯\_(ツ)_/¯' | pbcopy && echo 'Copied \"¯\_(ツ)_/¯\" to clipboard'"
# Backslashes and underscores must be doubly escaped if the text will be parsed as markdown
alias idke="echo -n '¯\\\\\\_(ツ)\_/¯' | pbcopy && echo 'Copied \"¯\\\\\_(ツ)\_/¯\" to clipboard'"
alias om="echo -n '¯\_( ˘͡ ˘̯)_/¯' | pbcopy && echo 'Copied \"¯\_( ˘͡ ˘̯)_/¯\" to clipboard'"
alias tableflip="echo -n '(╯°□°）╯︵ ┻━┻' | pbcopy && echo 'Copied \"(╯°□°）╯︵ ┻━┻\" to clipboard'"
alias muscles="echo -n 'ᕙ(⇀‸↼‶)ᕗ' | pbcopy && echo 'Copied \"ᕙ(⇀‸↼‶)ᕗ\" to clipboard'"
alias heyo="echo -n '(╭☞'ω')╭☞' | pbcopy && echo 'Copied \"(╭☞'ω')╭☞¯\" to clipboard'"
alias thanks="echo -n '(´▽\`ʃƪ)' | pbcopy && echo 'Copied \"(´▽\`ʃƪ)\" to clipboard'"


# Entertainment
alias tetris='emacs -q --no-splash -f tetris'




# and machine-specific aliases/overrides:
[ -f ~/.local-aliases.zsh ] && source ~/.local-aliases.zsh || true

# vim:foldmethod=marker:foldlevel=0
