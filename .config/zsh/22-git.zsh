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

# cf ./23-emacs-aliases.zsh for `magit` alias

# # TODO expand this into a proper little program
# subcommands:
#   ship it :: push current branch to its counterpart at origin
#   ship in :: pull --rebase
#   ship up :: rebase current branch on the repo's default branch at origin
#   ship out :: set upstream
alias shipit='ahoy && git push -u origin $(git rev-parse --abbrev-ref HEAD 2> /dev/null)'
alias SHIPIT='ahoy && git push --force-with-lease -u origin $(git rev-parse --abbrev-ref HEAD 2> /dev/null)'
# who doesn't love a good typo
alias SHIIT='ahoy && echo "      FUUCK"'

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
    git fetch origin
    git checkout $big_kahuna_branch
    git pull --ff-only
}

cob () {
  git checkout -b "`echo $* | tr ' ' -`"
}

d () {
  git diff --color "$@"
}

D () {
  if dots/at_hand difft; then
    git -c diff.external=difft diff --diff-algorithm=histogram --color "$@" | bat --plain
  elif dots/at_hand diff-so-fancy; then
    git diff --diff-algorithm=histogram --color "$@" | diff-so-fancy | bat --plain
  else
    git diff --diff-algorithm=histogram --color "$@"
  fi
}
alias gdc="d --cached"
alias gdo="git diff \$(git rev-parse --abbrev-ref HEAD 2> /dev/null)..origin/\$(git rev-parse --abbrev-ref HEAD 2> /dev/null)"

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

alias gp="git pull"
GP () {
  git branch --set-upstream-to=origin/$(git rev-parse --abbrev-ref HEAD) $(git rev-parse --abbrev-ref HEAD) && git pull "$@"
}

alias gpr="git pull --rebase"
alias GPR="GP --rebase"
alias gpf="git pull --ff-only"
alias GPF="GP --ff-only"

# TODO checkout, pull, and rebase all have a common ingredient: nacho code. Tug on that
# thread a little.
alias gr="git rebase"
alias gr-="git rebase -"
dots/clear_name grm
grm () {
  local big_kahuna_branch=$(git rev-parse --abbrev-ref origin/HEAD | cut -c8-)
  g fetch origin $big_kahuna_branch:$big_kahuna_branch
  # github's commit list orders by author date, not commit date; the `--reset-author-date`
  # flag can help avoid some Dumb Situations when rebasing on a busy upstream
  g rebase $big_kahuna_branch --reset-author-date
}
alias gri="g ri" # home-cooked git-ri, which simplifies syntax of `git rebase -i`

alias gb="git branch"
alias gbl="git branch -l"

l () {
  # no args
  if [[ $# -eq 0 ]]; then
    git log --oneline -8
  # one arg which is an integer
  elif [[ $# -eq 1 ]] && [[ "$1" = <-> ]]; then
    git log --oneline -$1
  # hopefully anything else is a valid set of additional git log args
  else
    git log --oneline "$@"
  fi
}
L () {
  # no args
  if [[ $# -eq 0 ]]; then
    git log --oneline --date=format:'%Y-%m-%d' --format='%C(yellow)%h%Creset [%C(blue)%cd%Creset] %s' -8
  # one arg which is an integer
  elif [[ $# -eq 1 ]] && [[ "$1" = <-> ]]; then
    git log --oneline --date=format:'%Y-%m-%d' --format='%C(yellow)%h%Creset [%C(blue)%cd%Creset] %s' -$1
  # hopefully anything else is a valid set of additional git log args
  else
    git log --oneline --date=format:'%Y-%m-%d' --format='%C(yellow)%h%Creset [%C(blue)%cd%Creset] %s' "$@"
  fi
}

LL () {
  # First we calculate how much space we have for the commit summary lines so we can
  # format the output nicely; this lets us easily scan the dates at the end of each line.
  # To do so, we measure the terminal width with `tput cols` and subtract the known length of
  # the prefix and suffix data, using the result unless it's shorter than a minimum
  # readable summary width: the sha, author, and date are only useful if we know which
  # commit they are associated with, after all.
  #
  # Why does prefix_length = 21?
  #   10 (length of commit short hash)
  # + 10 (space allotted for author name, truncating as needed)
  # + 2  (brackets around author name)
  # + 2  (one space after each datum)
  #
  # Why does suffix_length = 11?
  #   10 (length of YYYY-MM-DD)
  # + 1  (space)
  local summary_width=$(ruby -e '
term_width = `tput cols`.to_i
min = 40
max = 80
prefix_length = 26
suffix_length = 11
available_cols = term_width - prefix_length - suffix_length

width = if (available_cols > min and available_cols <= max)
          available_cols
        elsif available_cols > min
          max
        else
          min
        end

print width')

  # no args
  if [[ $# -eq 0 ]]; then
    git log --pretty="format:%C(auto)%h %C(auto)%<(12,trunc)[%an] %<($summary_width,trunc)%s %C(auto)%as" -8
  # one arg which is an integer
  elif [[ $# -eq 1 ]] && [[ "$1" = <-> ]]; then
    git log --pretty="format:%C(auto)%h %C(auto)%<(12,trunc)[%an] %<($summary_width,trunc)%s %C(auto)%as" -$1
  else
    git log --pretty="format:%C(auto)%h %C(auto)%<(12,trunc)[%an] %<($summary_width,trunc)%s %C(auto)%as" "$@"
  fi
}

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

alias gg="git grep"

alias b="git blame"

alias stash="git stash save -u"
alias pop="git stash pop"

ge () {
  local files
  local file_to_edit
  # store list of modified files in $files.
  # QUESTION: would an array work better than a multiline string?
  files=$(g | field 2)
  # if there's only one file in $files, just open that
  local file_count=$(echo $files | wc -l)
  if [[ $file_count -eq 1 ]]; then
    file_to_edit=$files
  else
    file_to_edit=$(echo $files | gum choose)
  fi

  if [ -n $file_to_edit ]; then
    e $file_to_edit
  fi
}

unique () {
  perl -ne '$H{$_}++ or print'
}
git-recent () {
  # lists unique git refs you have checked out, in order of how recently you checked them out
  git reflog | grep checkout: | awk '{print $6}' | unique
}
