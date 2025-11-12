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
