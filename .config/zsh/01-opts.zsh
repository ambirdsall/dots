setopt autocd extendedglob notify
unsetopt beep nomatch
setopt auto_cd PROMPT_SUBST

if [[ $OS_TYPE == "macos" ]]; then
    ulimit -n 2048
fi
