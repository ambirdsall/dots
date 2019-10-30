#
# any dumb and obvious stuff like printed output goes up top here
#

[[ -n $TMUX ]] && echo "Tmux sessions:"
[[ -n $TMUX ]] && tmux list-sessions
[[ -n $TMUX ]] && printf "\n"
[[ -z $TMUX ]] && printf "No tmux sessions right now!\n\n"

echo "Today in history:"
grep $(date +'%m/%d') < /usr/share/calendar/calendar.history | shuf | head -n 1
printf "\n"

#
# Actual config
#

# how on earth am I supposed to use a url bar I can't jump around with emacs shortcuts
gsettings set org.gnome.desktop.interface gtk-key-theme "Emacs"

export HISTCONTROL=ignorespace
# allow mv actions on multiple files, see:
autoload -U zmv
setopt auto_cd PROMPT_SUBST

# C-hjkl clobbers C-k, and kill-line is not negotiable if I'm using emacs bindings. So: vim.
bindkey -v
EDITOR='emacsclient -nw'

# repeated invocations of an xcape mapping without killing in between causes
# xcape to send multiples of the new mappings, which is terrible.
ps aux | grep -i '[x]cape' >/dev/null && killall -9 xcape
hash xcape 2>&1 >/dev/null && \
    xcape -e 'Control_L=Escape;Caps_Lock=Escape;Shift_L=Shift_L|9;Shift_R=Shift_R|0' >/dev/null

g () {
    if [ $# > 0 ]; then
        git "$@"
    else
        git status -s
    fi
}

strip_newline () {
    tr -d '\n'
}

# TODO fix handling of directories with spaces in their names
reverse_dir_stack () {
    dirs | awk '{ for (i=NF; i>1; i--) printf("%s %%F{238}᎒%%f", $i) }'
}

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

PS1='$(reverse_dir_stack)%F{cyan}%~%f $(current_branch)%F{136}$(current_commit)%f$(dirty_check) $(~/bin/moon-phase)
%(?.%F{239}.%F{196})ᐇ%f '

source /home/linuxbrew/.linuxbrew/opt/chruby/share/chruby/auto.sh
source /home/linuxbrew/.linuxbrew/opt/chruby/share/chruby/chruby.sh
# Aliases
[[ -a ~/aliases.zsh ]] && source ~/aliases.zsh
