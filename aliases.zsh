# * emacsclients
# TODO auto-start server, this is a solved problem lol
alias e="qed"
E () {
 emacsclient -c --alternate-editor="" "$@" & disown
}

# Don't want to mess with the existing session?
alias ee="emacs -nw"
EE () {
  emacs "$@" & disown
}

# sometimes in life, you don't have emacs installed on a new computer yet.
# These also tend to be times when you open config files a lot! And even when
# you do have emacs installed, cracking open a config/alias/whatever file is
# a scenario where falling back to vim is preferable to a slow-booting editor.
emv () {
  if command -v emacs > /dev/null; then
    emacsclient -nw --alternate-editor="vim" $@
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

# * pb{copy,paste}
# it's just a better clipboard API, tbh; plus muscle memory
if [ $(uname) != 'Darwin' ]; then
  if command -v xclip &>/dev/null; then
    alias pbcopy='xclip -i -selection clipboard'
    alias pbpaste='xclip -o -selection clipboard'
  elif command -v wl-copy &>/dev/null; then
    alias pbcopy='wl-copy'
    alias pbpaste='wl-paste --type text'
  fi
fi


# * cd to key directories from anywhere
# ** ~/c
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
# ** ~/.config
cd. () {
  if [[ $# -gt 0 ]]; then
    cdd ~/.config/${@}
  else
    cdd ~/.config
  fi
}
compdef '_files -W ~/.config' cd.

e. () {
  if [[ $# -gt 0 ]]; then
    e ~/.config/${@}
  else
    e ~/.config
  fi
}
compdef '_files -W ~/.config' e.

# * quick edit/source shell config files
alias ea="emv ~/aliases.zsh"
alias el="emv ~/.local-aliases.zsh"
alias sdf="source ~/aliases.zsh && echo sourced ~/aliases.zsh && [[ -f ~/.local-aliases.zsh ]] && source ~/.local-aliases.zsh && echo sourced ~/.local-aliases.zsh || true"
alias zrc="emv ~/.zshrc"
alias rc="source ~/.zshrc"
alias et="emv ~/.tmux.conf"

# * serve local files
# TODO change to ~npx live-server~ or whatever
alias serve="python -m SimpleHTTPServer"

# * text formatting all fucked up in the terminal?
alias sane='stty sane'

# * task-based shortcuts
# ** first you have to be able to get where you're going
# ** jump around

alias um="find . -type f | grep -vE '.tmp-*|.git|node_modules|bower_components' | fzf --multi --preview 'bat --color=always {}'"
cdf() {
  pushd $(dirname $(find . -type f | grep -vE '.tmp-*|.git|node_modules|bower_components|DS_Store' | fzf --preview 'bat --color=always {}'))
}
# ** fuzzy find from chrome history and open url (mac-specific for now)
if [ $(uname) == 'Darwin' ]; then
    h () {
        local cols sep
        cols=$(( COLUMNS / 3 ))
        sep='{::}'

        # TODO: find chrome history's location on linux
        cp -f ~/Library/Application\ Support/Google/Chrome/Default/History /tmp/h

        sqlite3 -separator $sep /tmp/h \
                "select substr(title, 1, $cols), url
     from urls order by last_visit_time desc" |
            awk -F $sep '{printf "%-'$cols's  \x1b[36m%s\x1b[m\n", $1, $2}' |
            fzf --ansi --multi | sed 's#.*\(https*://\)#\1#' | xargs open
    }
fi



# ** display the source code of a script
showme () {
  bat $(which $1)
}
# * program-based shortcuts
# ** shortcuts for standard commands and builtins
# ** awk
alias awkcsv='awk -F "\"*,\"*"'

# ** background jobs & processes
# these play nicely with fg (especially with vim/emacsclient)
alias j=jobs
# Typing the percent sign gets annoying fast when you run `kill` all the time with `%n`-style arguments on suspended `jobs`
k () {
    kill %"$1"
}

# ** cd by any name would smell as sweet
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

# ** interactively choose command from history
r () {
    $(fc -l -n 1 | tail -r | fzf)
}

# ** chmod
alias cx='chmod +x'

# ** clear
alias clear='clear; [[ -z "$TMUX" ]] && tls 2>/dev/null || true'

# ** = :: less
= () {
    # iff there are 0 arguments given, assume input from stdin (i.e. a pipe)
    if [[ $# -gt 0 ]]; then
        /usr/share/vim/vim80/macros/less.sh "$*"
    else
        /usr/share/vim/vim80/macros/less.sh "$*" -
    fi
}

# ** ls
alias ls="ls -GF"
alias la="ls -A"
alias ll="ls -l"

# allow exiting with C-c
alias sl="sl -e"

# ** mkdir
alias mkdir="mkdir -pv"

# ** rm
alias fuck="rm -rf"

# ** tail
tailfh () {
  tail -f $1 | ack -i 'error' --passthru
}

# ** mv
alias mmv='noglob zmv -W'

# ** sudo
alias please=sudo

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

# ** shortcuts for nonstandard commands
# ** tmux
alias t=tmux
alias ta="tmux attach -t"
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
alias s=scripts
run-npm-script () {
  local run=$(if test -f pnpm-lock.yaml; then echo pnpm; else echo npm run; fi)

  # using separate sed invocations instead of something simpler like `tr -d` to
  # avoid mangling script keys containing colons
  local chosen_script=$(scripts | grep -Eo '".+": ' | sed 's/"//' | sed 's/": //' | fzf)

  test -n $chosen_script && $run $chosen_script
}
alias S=run-npm-script
# * Git gets its own top-level section
# ** porcelain
# *** g is for git, that's good enough for me
# No arguments: `git status`
# With arguments: acts like `git`
g () {
  if [[ $# -gt 0 ]]; then
    git "$@"
  else
    git status -s
  fi
}
# Complete g like git
compdef g=git

# *** oh, oh, it's ~magit~
# TODO expand into a proper elisp script
gs () {
    emacsclient --socket=magit -nw -e "
(progn
  (or (advice-member-p 'save-buffers-kill-terminal '+magit/quit)
      (advice-add '+magit/quit :before 'save-buffers-kill-terminal))
  (magit-status))" || emacs --daemon=magit
}

# ** git operations
# ** ship shape
# # TODO expand this into a proper little program
# subcommands:
#   ship it :: push current branch to its counterpart at origin
#   ship in :: pull --rebase
#   ship up :: rebase current branch on the repo's default branch at origin
#   ship out :: set upstream
alias ahoy='echo "       _~\n    _~ )_)_~\n    )_))_))_)\n    _!__!__!_\n    \______t/\n  ~~~~~~~~~~~~~"'
alias shipit='ahoy && git push origin $(git rev-parse --abbrev-ref HEAD 2> /dev/null)'
alias SHIPIT='ahoy && git push --force-with-lease origin $(git rev-parse --abbrev-ref HEAD 2> /dev/null)'
# who doesn't love a good typo
alias SHIIT='ahoy && echo "      FUUCK"'

# *** clone
# run `git clone` and `cdd` into dir
# if no arguments are provided, assumes you have copied a repo url to your clipboard
gc () {
  if [[ $# -eq 0 ]]; then
    repo=$(pbpaste)
  else
    repo=$@
  fi

  repo_dir_with_trailing_git=${repo##*/}
  repo_dir=${repo_dir_with_trailing_git%.git}
  git clone $repo
  echo ""
  cdd $repo_dir
}

# *** checkout
co () {
  if [[ $# -gt 0 ]]; then
    git checkout "$@"
  else
    local BRANCH_OR_REF=$(git recent | fzf --preview 'git show heads/{} | diff-so-fancy')
    if [ ! -z $BRANCH_OR_REF ]; then
        git checkout $BRANCH_OR_REF
    fi
  fi
}
alias co-="git checkout -"

cor () {
    local BRANCH_OR_REF=$(git recent "$@" | fzf --preview 'git show heads/{} | diff-so-fancy')
    if [ ! -z $BRANCH_OR_REF ]; then
        git checkout $BRANCH_OR_REF
    fi
}

com () {
    local big_kahuna_branch=$(git rev-parse --abbrev-ref origin/HEAD | cut -c8-)

    g co $big_kahuna_branch && g pull --rebase
}

cob () {
  git checkout -b "`echo $* | tr ' ' -`"
}

# *** diff
d () {
  git diff --diff-algorithm=minimal --color "$@" | diff-so-fancy | bat --plain
}
alias gdc="d --cached"
alias gdo="git diff \$(git rev-parse --abbrev-ref HEAD 2> /dev/null)..origin/\$(git rev-parse --abbrev-ref HEAD 2> /dev/null)"

# *** add
alias p="git add -p"

# *** commit
c () {
  if [[ $# -gt 0 ]]; then
    git commit -m "$*"
  else
    git commit -v
  fi
}
alias a="git commit --amend"
alias arh="git commit --amend --reuse-message=HEAD"

# *** fetch/pull
alias f="git fetch"

alias gp="git pull"
GP () {
  git branch --set-upstream-to=origin/$(git rev-parse --abbrev-ref HEAD) $(git rev-parse --abbrev-ref HEAD) && git pull "$@"
}

alias gpr="git pull --rebase"
alias GPR="GP --rebase"
alias gpf="git pull --ff-only"
alias GPF="GP --ff-only"

# *** rebase
# TODO checkout, pull, and rebase all have a common ingredient: nacho code. Tug on that
# thread a little.
alias gr="git rebase"
alias gr-="git rebase -"
alias grm="com && co- && gr-"
alias gri="g ri" # home-cooked git-ri, which simplifies syntax of `git rebase -i`

# *** branch
alias gb="git branch"
alias gbl="git branch -l"

# *** log
l () {
  # no args
  if [[ $# -eq 0 ]]; then
    git log --oneline --decorate -8
  # one arg which is an integer
  elif [[ $# -eq 1 ]] && [[ "$1" = <-> ]]; then
    git log --oneline --decorate -$1
  else
    echo "Usage: l [number of commits]" >&2
  fi
}

L () {
  # no args
  if [[ $# -eq 0 ]]; then
    git log --pretty=reference --decorate -8
  # one arg which is an integer
  elif [[ $# -eq 1 ]] && [[ "$1" = <-> ]]; then
    git log --pretty=reference --decorate -$1
  else
    echo "Usage: l [number of commits]" >&2
  fi
}

command -v ruby &>/dev/null && eval "$(ruby -e '9.times do |i| puts %Q{alias l#{i+1}=l\\ -#{i+1}} end')"
alias lg="git log --pretty=reference --decorate --graph --all"
alias rl="git reflog"

describe-commits () {
  if [[ $# -eq 1 ]] && [[ "$1" = <-> ]]; then
    # 1) list the sha and subject line for the latest ARG commits
    # 2) pipe that to an awk script which formats each as "- $SHA :: $SUBJECT_LINE\n"
    # 3) pipe that to tac to reverse the order, listing commits from first to latest
    # then copy-paste that into your pull request description, editing the subject lines into nice
    # descriptions as needed
    git log --oneline -$1 --no-decorate \
      | awk '{printf "- " $1 " :: "; for (i=2; i<=NF; i++) printf $i FS; print ""}' \
      | tac
  else
    echo "Usage: describe-commits [number of commits]" >&2
  fi
}

# *** grep
alias gg="git grep"

# *** blame
alias b="git blame"

# *** stash
alias stash="git stash save -u"
alias pop="git stash pop"

# ** helper functions
unique () {
  perl -ne '$H{$_}++ or print'
}
git-recent () {
  # lists unique git refs you have checked out, in order of how recently you checked them out
  git reflog | grep checkout: | awk '{print $6}' | unique
}
# * Self-expanding shell abbreviations
# cf. http://zshwiki.org/home/examples/zleiab
typeset -Ag abbreviations
abbreviations=(
"pa"    "| awk"
"pag"   "| agrep"
"pb"    "| bc"
"peg"   "| egrep"
"pgr"   "| groff -s -p -t -e -Tlatin1 -mandoc"
"pf"    "| fzf"
"pg"    "| rg -S"
"G"     "| rg -S"
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
